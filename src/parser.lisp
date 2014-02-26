(in-package :6502)

(defun make-stream (text)
  "Make a string displaced onto the given text."
  (make-array (length text) :element-type 'character
              :displaced-to text :displaced-index-offset 0))

(defun try-fetch (stream regex)
  "If the stream begins with a regex match, returns the matched text and move
   the stream past it. Otherwise, returns nil."
  (let ((result (cl-ppcre:scan-to-strings regex stream)))
    (when result
      (multiple-value-bind (original index-offset) (array-displacement stream)
        (adjust-array stream (- (length stream) (length result))
                      :displaced-to original
                      :displaced-index-offset (+ index-offset (length result))))
      result)))

(defun substream-advance (stream start end)
  "Set the stream to a substream at START positions ahead and finishing
   at END position."
  (multiple-value-bind (original index-offset) (array-displacement stream)
    (unless original
      (error "substream-advance called with a string, not a stream"))
    (adjust-array stream (- end start) :displaced-to original
                  :displaced-index-offset (+ index-offset start))))

(defun skip-white-space (stream)
  "Fetches white space from the stream, ignores it and returns the stream."
  (try-fetch stream "^\\s+")
  stream)

(defun fetch-label (stream)
  "Fetches a label from the stream, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z][a-zA-Z0-9_]*:")))
    (when result (string-right-trim ":" result))))

(defun fetch-opcode (stream)
  "Fetches an opcode from the stream as a keyword, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z]{3}")))
    (when result (intern (string-upcase result) :keyword))))

(defun fetch-literal (stream)
  "Fetches a literal value from the stream and returns it as an alist
   containing the integer value and address mode, or returns nil."
  (let ((result (try-fetch stream "^((\\$|\\&)[a-fA-F0-9]+|%[0-1]+|[0-9]+)")))
    (cond
      ((not result) nil)
      ((find (aref result 0) "$&") (parse-integer (subseq result 1) :radix 16))
      ((char= (aref result 0) #\%) (parse-integer (subseq result 1) :radix 2))
      (t (parse-integer result :radix 10)))))

(defun fetch-name (stream)
  "Fetches a name from the stream, or returns nil."
  (try-fetch stream "^[a-zA-Z][a-zA-Z0-9_]*"))

(defun fetch-term (stream)
  "Fetches a literal or name from the stream, or returns nil."
  (or (fetch-literal stream) (fetch-name stream)))

(defun match-operand-address-modes (stream)
  "Matches the stream against all address-mode readers, returning those that
   match, as well as the positions where the match occurs."
  (let ((value-match (list nil nil)))
    (list (loop for address-mode being the hash-keys of *address-modes*
             when (multiple-value-bind (start end match-start match-end)
                      (cl-ppcre:scan (reader address-mode) stream)
                    (when start
                      (setf value-match (list match-start match-end))))
             collect address-mode) value-match)))

(defun operand-possible-modes-and-value (stream)
  "Returns all matching address-modes for the operand, along with positions
   where the match occurs."
  (destructuring-bind (address-modes (match-starts match-ends))
      (match-operand-address-modes stream)
    (cond
      ((find 'implied address-modes) (list (list 'implied) 0 0))
      ((find 'accumulator address-modes) (list (list 'accumulator) 0 1))
      (t (list address-modes (aref match-starts 0) (aref match-ends 0))))))

(defun fetch-expression (stream)
  "Fetches an expression from the stream, either a term or a term plus another."
  (let ((term-1 (fetch-term stream)))
    (if (try-fetch (skip-white-space stream) "^\\+")
        (list '+ term-1 (fetch-expression stream))
        term-1)))

(defun fetch-operand (stream)
  "Fetches the operand, returning its numerical value and possible
   address-modes."
  (destructuring-bind (possible-modes value-start value-end)
      (operand-possible-modes-and-value stream)
    (substream-advance stream value-start value-end)
    (list (fetch-expression stream) possible-modes)))

(defun parse-line (text)
  "Converts a line of text into an instruction representing the assembly code."
  (let* ((stream (make-stream text))
         (label (fetch-label (skip-white-space stream)))
         (opcode (fetch-opcode (skip-white-space stream)))
         (operand (fetch-operand (skip-white-space stream)))
         (value (first operand))
         (address-modes (second operand)))
    (make-instruction :label label :opcode opcode :value value
                      :address-mode address-modes)))

(defun strip-comment (text)
  "Removes comment and white space from end of string."
  (let ((pos (position #\; text)))
    (when pos (setf text (subseq text 0 pos))))
  (string-right-trim " " text))

(defun parse-code (text)
  "Parses the assembly source text and returns the assembled code as a list of
   alists."
  (loop for line in (cl-ppcre:split "\\n" text)
        when (parse-line (strip-comment line)) collect it))
