(in-package :6502)

(defun make-alist (&rest rest)
  "Creates an alist with the given keys and values."
  (loop for (key val) on rest by #'cddr when val collect (cons key val)))

(defmacro try-fetch (stream regex)
  "If the stream begins with a regex match, returns the matched text and move
   the stream past it. Otherwise, returns nil."
  (let ((text (gensym))
        (result (gensym)))
    `(let* ((,text (first ,stream))
            (,result (cl-ppcre:scan-to-strings ,regex ,text)))
       (when ,result
         (setf (first ,stream) (subseq ,text (length ,result)))
         ,result))))

(defun try-get-label (stream)
  "Fetches a label from the stream, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z][a-zA-Z0-9_]*:")))
    (when result (string-right-trim ":" result))))

(defun try-get-white-space (stream)
  "Fetches white space from the stream, or returns nil."
  (try-fetch stream "^\\s+"))

(defun try-get-opcode (stream)
  "Fetches an opcode from the stream as a keyword, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z]+")))
    (when result (intern (string-upcase result) :keyword))))

(defun try-get-literal (stream)
  "Fetches a literal value from the stream and returns it as an alist
   containing the integer value and address mode, or returns nil."
  (let ((result (try-fetch stream "^#?(\\$[a-fA-F0-9]+|%[0-1]+|[0-9]+)"))
        value address-mode)
    (when result
      (when (char= (aref result 0) #\#)
        (setf result (subseq result 1))
        (setf address-mode :immediate))
      (cond
        ((char= (aref result 0) #\$)
         (setf value (parse-integer (subseq result 1) :radix 16)))
        ((char= (aref result 0) #\%)
         (setf value (parse-integer (subseq result 1) :radix 2)))
        (t
         (setf value (parse-integer result :radix 10))))
      (make-alist :value value :address-mode address-mode))))

(defun try-get-name (stream)
  "Fetches a name from the stream, or returns nil."
  (let ((result (try-fetch stream "^#?[a-zA-Z][a-zA-Z0-9_]*"))
        address-mode)
    (when result
      (when (char= (aref result 0) #\#)
        (setf result (subseq result 1))
        (setf address-mode :immediate))
      (make-alist :value result :address-mode address-mode))))

(defun try-get-literal-or-name (stream)
  "Fetches a literal or name from the stream, or returns nil."
  (or (try-get-literal stream) (try-get-name stream)))

(defun try-get-indirect (stream)
  "Fetches an indirect operand from the stream, or returns nil."
  (when (try-fetch stream "^\\(")
    (try-get-white-space stream)
    (let ((operand (try-get-literal-or-name stream)))
      (try-get-white-space stream)
      (unless operand
        (error "Failed to parse"))
      (when (eq (cdr (assoc :address-mode operand)) :immediate)
        (error "Cannot use indirect mode with immediate operand"))
      (unless (try-fetch stream "^\\)")
        (error "Expected close of parenthesis for indirect mode"))
      (append operand (make-alist :address-mode :indirect)))))

(defun try-get-indexing-mode (stream)
  "Fetches the indexing mode from the stream, or returns nil."
  (when (try-fetch stream "^,")
    (try-get-white-space stream)
    (cond
      ((try-fetch stream "^(x|X)") :x)
      ((try-fetch stream "^(y|Y)") :y))))

(defun try-get-comment (stream)
  "Fetches a comment from the stream, or returns nil."
  (try-fetch stream "^;.*$"))

(defun parse-term (stream)
  "Parse a basic term."
  (try-get-white-space stream)
  (try-get-literal-or-name stream))

(defun parse-expression (stream)
  "Parse an expression, which is either a term or a sum of a term and another
   expression."
  (let ((expr-1 (parse-term stream)))
    (try-get-white-space stream)
    (if (try-fetch stream "^\\+")
        (let ((expr-2 (parse-expression stream)))
          (list (cons :value (list '+ (cdr (assoc :value expr-1))
                                      (cdr (assoc :value expr-2))))))
        expr-1)))

(defun parse-operand (stream)
  "Parse an operand, which is either an expression, or an indirect access to an
   expression."
  (if (try-fetch stream "^\\(")
      (progn
        (try-get-white-space stream)
        (let ((operand (parse-expression stream)))
          (unless operand
            (error "Failed to parse"))
          (when (eq (cdr (assoc :address-mode operand)) :immediate)
            (error "Cannot use indirect mode with immediate operand"))
          (unless (try-fetch stream "^\\)")
            (error "Expected close of parenthesis for indirect mode"))
          (append operand (make-alist :address-mode :indirect))))
      (parse-expression stream)))

(defun parse-line (text)
  "Converts a line of text into an alist representing the assembly code."
  (let* ((stream (list text))
         label opcode operand indexing)
    (setf label (try-get-label stream))
    (try-get-white-space stream)
    (setf opcode (try-get-opcode stream))
    (try-get-white-space stream)
    (setf operand (parse-operand stream))
    (try-get-white-space stream)
    (setf indexing (try-get-indexing-mode stream))
    (try-get-white-space stream)
    (try-get-comment stream)
    (when (not (string= (first stream) ""))
      (error (format nil "Garbage text in line: ~s" (first stream))))
    (when (or label opcode operand)
      (append operand (make-alist :label label :opcode opcode
                                  :indexing indexing)))))

(defun parse-code (text)
  "Parses the assembly source text and returns the assembled code as a list of
   alists."
  (loop for line in (cl-ppcre:split "\\n" text)
        when (parse-line line) collect it))
