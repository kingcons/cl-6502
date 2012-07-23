(in-package :6502-cpu)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defvar *symtable* (make-hash-table :test #'equal)
  "A symbol table for use during assembly.")

(defun trim-whitespace (str)
  "Remove trailing and leading Tabs, Returns, and Spaces from STR."
  (string-trim '(#\Tab #\Return #\Space) str))

(defun extract-code (src)
  "Trim whitespace and remove comments from a given line of code, SRC."
  (let ((trimmed-src (trim-whitespace src)))
    (trim-whitespace (subseq trimmed-src 0 (position #\; trimmed-src)))))

(defun extract-num (str &optional (mode 'implied))
  "Extract a hex number from its containing string, STR."
  (let ((token (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" str)))
    (flet ((parse-hex (x) (parse-integer x :radix 16)))
      (when token
        (if (member mode '(absolute absolute-x absolute-y indirect))
            (mapcar #'parse-hex (list (subseq token 2) (subseq token 0 2)))
            (parse-hex token))))))

(defun split-lines (text)
  "Split TEXT by newlines into a list of strings."
  (cl-ppcre:split "\\n" text))

(defun tokenize (line)
  "Split a LINE by spaces into its constituent tokens."
  (cl-ppcre:split "\\ " line))

(defun preprocess-p (line)
  "Take a LINE of source and return T if it contains a preprocessor statement."
  (or (position #\: line) (position #\= line)))

(defun preprocess (line pc)
  "Preprocess the line setting values in the *SYMTABLE* accordingly."
  (flet ((set-var (delimiter &optional val)
           (destructuring-bind (var &rest src) (cl-ppcre:split delimiter line)
             (setf (gethash var *symtable*) (or val (first src))))))
    (cond ((position #\: line) (set-var "\\:" (format nil "~4,'0x" pc)))
          ((position #\= line) (set-var "\\=")))))

(defmethod next-pc ((line vector) pc)
  (+ pc (length line)))

(defmethod next-pc ((line list) pc)
  (let* ((name (first line))
         (mode (and name (match-mode (rest line)))))
    (if mode
        (+ pc (third (aref *opcodes* (find-opcode name mode))))
        (+ pc 3))))

(defmethod next-pc ((line string) pc)
  (if (or (preprocess-p line) (emptyp line))
      pc
      (next-pc (tokenize line) pc)))

(defun asm (source)
  "Assemble SOURCE string into a bytevector and return it."
  (clrhash *symtable*)
  (let ((results (loop for pc = 0 then (next-pc line pc)
                    for line in (mapcar #'extract-code (split-lines source))
                    if (preprocess-p line) do (preprocess line pc)
                    else unless (emptyp line)
                    collect (process-tokens (tokenize line)))))
    (apply 'concatenate 'vector (resolve-syms (remove nil results)))))

(defun find-opcode (name mode)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise."
  (flet ((match (opcode)
           (let ((op (aref *opcodes* opcode)))
             (and (string-equal name (symbol-name (first op)))
                  (eql mode (second (alexandria:lastcar op))) opcode))))
    (loop for op from 0 to 255 thereis (match op)
       finally (error 'illegal-opcode :opcode (list name mode)))))

(defun match-mode (tokens)
  "Given a list of args, TOKENS, match them to an addressing mode or return NIL."
  (let ((regex-modes '(("^$" . implied)
                       ("^[aA]$" . accumulator)
                       ("^#\\$[0-9a-fA-F]{2}$" . immediate)
                       ("^\\$[0-9a-fA-F]{4},[xX]$" . absolute-x)
                       ("^\\$[0-9a-fA-F]{4},[yY]$" . absolute-y)
                       ("^\\$[0-9a-fA-F]{4}$" . absolute)
                       ("^\\$[0-9a-fA-F]{2},[xX]$" . zero-page-x)
                       ("^\\$[0-9a-fA-F]{2},[yY]$" . zero-page-y)
                       ("^\\$[0-9a-fA-F]{2}$" . zero-page)
                       ("^\\(\\$[0-9a-fA-F]{2}\\),[xX]$" . indirect-x)
                       ("^\\(\\$[0-9a-fA-F]{2}\\),[yY]$" . indirect-y)
                       ("^\\(\\$[0-9a-fA-F]{4}\\)$" . indirect)
                       ("^&[0-9a-fA-F]{2,4}$" . relative))))
    (let ((line (apply 'concatenate 'string tokens)))
      (loop for (regex . mode) in regex-modes
         when (cl-ppcre:scan regex line) return mode))))

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
  (let* ((result (format nil "~a~4,'0x" prefix val))
         (label (extract-num result))
         (mode (match-mode (list result))))
    (if (eql mode 'relative)
        (format nil "~a~4,'0x" prefix (compute-offset label pc))
        result)))

(defun resolve (tokens pc)
  "Given TOKENS, resolve symbols and return a vector, or raise INVALID-SYNTAX."
  (flet ((lookup (sym)
           (gethash (string-trim '(#\# #\$ #\( #\&) sym) *symtable*)))
    (unless (some #'lookup tokens)
      (error 'invalid-syntax :tokens tokens))
    (loop for token in tokens for sym = (lookup token)
       if sym collect (splice-sym sym (prefix-of token) (1+ pc))
       else collect token)))

(defun resolve-syms (seqs)
  "Take a list of tokens and vectors, SEQS, converting any tokens to vectors."
  (loop for pc = 0 then (next-pc item pc)
     for item in seqs
     collect (etypecase item
               (vector item)
               (list (process-tokens (resolve item pc))))))

(defun process-args (tokens mode)
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  (remove nil (flatten (mapcar (lambda (x) (extract-num x mode)) tokens))))

(defun process-tokens (tokens)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let ((mode (match-mode (rest tokens))))
    (if mode
        (destructuring-bind (mnemonic &rest args) tokens
          (apply 'vector (find-opcode mnemonic mode) (process-args args mode)))
        tokens)))
