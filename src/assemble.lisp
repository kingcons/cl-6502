(in-package :6502)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defvar *symtable* (make-hash-table :test #'equal)
  "A symbol table for use during assembly.")

(defun extract-num (str &optional (mode 'implied))
  "Extract a hex number from its containing string, STR."
  (let ((token (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" str)))
    (flet ((parse-hex (x) (parse-integer x :radix 16)))
      (when token
        (if (member mode '(absolute absolute-x absolute-y indirect))
            (mapcar #'parse-hex (list (subseq token 2) (subseq token 0 2)))
            (parse-hex token))))))

(defun tokenize (line)
  "Split a LINE by spaces into its constituent tokens, removing comments."
  (let ((code (subseq line 0 (position #\; line))))
    (cl-ppcre:split "\\ " (string-trim '(#\Tab #\Return #\Space) code))))

(defun preprocess-p (tokens)
  "Take a list of TOKENS and return T if it contains a preprocessor statement."
  (some (lambda (x) (search x (first tokens))) '(":" "=")))

(defun preprocess (statement pc)
  "Preprocess the STATEMENT setting values in the *SYMTABLE* accordingly."
  (flet ((set-var (delimiter &optional val)
           (destructuring-bind (var &rest src) (cl-ppcre:split delimiter statement)
             (setf (gethash var *symtable*) (or val (first src))))))
    (cond ((search ":" statement) (set-var "\\:" pc))
          ((search "=" statement) (set-var "\\="))
          (t (error 'invalid-syntax :line statement)))))

(defun next-pc (line pc)
  "Compute the new program counter given LINE and PC."
  (let ((mode (match-mode (rest line))))
    (cond ((or (preprocess-p line) (emptyp line)) pc)
          (mode (+ pc (third (find-opcode (first line) mode t))))
          ((some (lambda (x) (search x (second line))) '("&" "#$")) (+ pc 2))
          (t (+ pc 3)))))

(defmacro with-src-pass ((src) &body body)
  "Loop over SRC, tracking the PC and binding LINE. BODY should be a LOOP expr."
  `(loop for pc = 0 then (next-pc line pc)
      for line in (mapcar #'tokenize (cl-ppcre:split "\\n" ,src))
        ,@body))

(defun asm (source)
  "Assemble SOURCE string into a bytevector and return it."
  (clrhash *symtable*)
  (with-src-pass (source) if (preprocess-p line) do (preprocess (first line) pc))
  (let ((results (with-src-pass (source)
                   unless (or (preprocess-p line) (emptyp line))
                   collect (process-tokens line pc))))
    (apply 'concatenate 'vector results)))

(defun find-opcode (name mode &optional return-match-p)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise.
If RETURN-MATCH-P is non-nil, return the match directly rather than its index."
  (let* ((insts (remove-if-not (lambda (x) (eql mode x)) *opcodes* :key #'fourth))
         (match (find (intern (string-upcase name) :6502) insts :key #'first)))
    (cond ((cl:and match return-match-p) match)
          (match (position match *opcodes*))
          (t (error 'illegal-opcode :opcode (list name mode))))))

(defun match-mode (tokens)
  "Given a list of args, TOKENS, match them to an addressing mode or return NIL."
  (let ((line (apply 'concatenate 'string tokens)))
    (loop for mode in (remove-duplicates (map 'list #'fourth *opcodes*))
       when (cl:and mode (cl-ppcre:scan (reader mode) line)) return mode)))

(defun prefix-of (str)
  "Extract the mode prefix of STR."
  (subseq str 0 (1+ (loop for char in '(#\# #\$ #\( #\&)
                     maximizing (or (position char str) 0)))))

(defun compute-offset (label pc)
  "Compute the offset for a relative branch from PC to LABEL."
  (wrap-byte (if (> label pc)
                 (- label pc)
                 (- #xff (- pc label)))))

(defun splice-sym (val prefix pc)
  "Splice together VAL and PREFIX, computing an offset if needed."
  (let ((spliced (if (string= prefix "&")
                     (format nil "~2,'0x" (compute-offset val pc))
                     (format nil "~4,'0x" val))))
    (format nil "~a~a" prefix spliced)))

(defun resolve (tokens pc)
  "Given TOKENS and PC, resolve any symbols in TOKENS."
  (flet ((lookup (sym)
           (gethash (string-trim '(#\# #\$ #\( #\&) sym) *symtable*)))
    (loop for token in tokens for sym = (lookup token)
       if sym collect (splice-sym sym (prefix-of token) (1+ pc))
       else collect token)))

(defun process-args (tokens mode)
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  (remove nil (flatten (mapcar (lambda (x) (extract-num x mode)) tokens))))

(defun process-tokens (tokens pc)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let* ((args (resolve (rest tokens) pc))
         (mode (match-mode args)))
    (if mode
        (apply 'vector (find-opcode (first tokens) mode) (process-args args mode))
        (error 'invalid-syntax :line (format nil "~{~A~^ ~}" tokens)))))
