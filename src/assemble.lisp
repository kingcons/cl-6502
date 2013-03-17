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

(defgeneric asm (source)
  (:documentation "Assemble SOURCE into a bytevector and return it.")
  (:method :before (source) (clrhash *symtable*)))

(defun process-statement (statement)
  "Given a symbolic assembly STATEMENT, convert it to a list of bytes."
  (destructuring-bind (op &rest args)
      (mapcar (compose 'string-upcase 'symbol-name) statement)
    (let ((mode (match-mode args :sexp)))
      (list* (find-opcode op mode) (process-args args mode)))))

(defmethod asm ((source list))
  (apply 'concatenate 'vector (mapcar #'process-statement source)))

(defmacro with-src-pass ((src) &body body)
  "Loop over SRC binding LINE. BODY should be a LOOP expr."
  `(loop for line in (mapcar #'tokenize (cl-ppcre:split "\\n" ,src))
        ,@body))

(defmethod asm ((source string))
  (let ((results (with-src-pass (source)
                   unless (emptyp line)
                   collect (process-tokens line))))
    (apply 'concatenate 'vector results)))

(defun find-opcode (name mode &optional return-match-p)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise.
If RETURN-MATCH-P is non-nil, return the match directly rather than its index."
  (let* ((insts (remove-if-not (lambda (x) (eql mode x)) *opcodes* :key #'fourth))
         (match (find (intern (string-upcase name) :6502) insts :key #'first)))
    (cond ((cl:and match return-match-p) match)
          (match (position match *opcodes*))
          (t (error 'illegal-opcode :opcode (list name mode))))))

(defun transform-sexp-syntax (tokens)
  "Given a SEXP-token using an indirect, *.x or *.y addressing mode, transform
it to use the classic string assembly syntax."
  (flet ((munge-indirect (x)
           (cl-ppcre:regex-replace "\@([^.]*)(.*)?" x "($\\1)\\2")))
    (mapcar (lambda (x) (substitute #\, #\. (munge-indirect x))) tokens)))

(defun match-mode (tokens &optional sexp-syntax-p)
  "Given a list of args, TOKENS, match them to an addressing mode or return NIL.
SEXP-SYNTAX-P should be t when tokens are in assembler's SEXP-based syntax."
  (when sexp-syntax-p
    (setf tokens (transform-sexp-syntax tokens)))
  (let ((line (apply 'concatenate 'string tokens)))
    (loop for mode in (remove-duplicates (map 'list #'fourth *opcodes*))
       when (cl:and mode (cl-ppcre:scan (reader mode) line)) return mode)))

(defun process-args (tokens mode)
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  (remove nil (flatten (mapcar (lambda (x) (extract-num x mode)) tokens))))

(defun process-tokens (tokens)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let* ((args (rest tokens))
         (mode (match-mode args)))
    (if mode
        (apply 'vector (find-opcode (first tokens) mode) (process-args args mode))
        (error 'invalid-syntax :line (format nil "~{~A~^ ~}" tokens)))))
