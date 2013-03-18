(in-package :6502)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defgeneric asm (source)
  (:documentation "Assemble SOURCE into a bytevector and return it."))

(defmethod asm ((source list))
  (if (listp (first source))
      (apply 'concatenate 'vector (mapcar #'process-statement source))
      (apply 'vector (process-statement source))))

(defun process-statement (statement)
  "Given a symbolic assembly STATEMENT, convert it to a list of bytes."
  (destructuring-bind (op &rest args)
      (mapcar (compose 'string-upcase 'symbol-name) statement)
    (if (> (length args) 1)
        (error 'invalid-syntax :line statement)
        (let ((mode (match-mode args)))
          (list* (find-opcode op mode) (process-args args mode))))))

(defun find-opcode (name mode)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise."
  (let* ((insts (remove-if-not (lambda (x) (eql mode x)) *opcodes* :key #'fourth))
         (match (find (intern (string-upcase name) :6502) insts :key #'first)))
    (if match
        (position match *opcodes*)
        (error 'illegal-opcode :opcode (list name mode)))))

(defun transform-sexp-syntax (tokens)
  "Given a SEXP-token using an indirect, *.x or *.y addressing mode, transform
it to use the classic string assembly syntax."
  (flet ((munge-indirect (x)
           (cl-ppcre:regex-replace "\@([^.]*)(.*)?" x "($\\1)\\2")))
    (mapcar (lambda (x) (substitute #\, #\. (munge-indirect x))) tokens)))

(defun match-mode (tokens)
  "Given a list of args, TOKENS, match them to an addressing mode or return NIL."
  (let ((line (apply 'concatenate 'string (transform-sexp-syntax tokens))))
    (loop for mode in (remove-duplicates (map 'list #'fourth *opcodes*))
       when (cl:and mode (cl-ppcre:scan (reader mode) line)) return mode)))

(defun extract-num (str &optional (mode 'implied))
  "Extract a hex number from its containing string, STR."
  (let ((token (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" str)))
    (flet ((parse-hex (x) (parse-integer x :radix 16)))
      (when token
        (if (member mode '(absolute absolute-x absolute-y indirect))
            (mapcar #'parse-hex (list (subseq token 2) (subseq token 0 2)))
            (parse-hex token))))))

(defun process-args (tokens mode)
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  (remove nil (flatten (mapcar (lambda (x) (extract-num x mode)) tokens))))

(defun trim-whitespace (str)
  (let ((code (subseq str 0 (position #\; str))))
    (string-trim '(#\Tab #\Return #\Space) code)))

(defun tokenize-statement (str)
  (let ((results (cl-ppcre:split " " (trim-whitespace str))))
    (mapcar (lambda (x) (intern x :keyword)) results)))

(defmethod asm ((source string))
  (let ((tokenized (mapcar #'tokenize-statement (cl-ppcre:split "\\n" source))))
    (asm (remove nil tokenized))))
