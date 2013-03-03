(in-package :6502)

(defun disasm-ins (index &optional (disasm-op #'print-instruction))
    "Lookup the metadata for the instruction at INDEX and pass it to
DISASM-OP for formatting and display, returning the instruction length."
  (destructuring-bind (name cycles length mode) (aref *opcodes* (get-byte index))
    (declare (ignore cycles))
    (let ((code-block (coerce (get-range index (+ index length)) 'list)))
      (list length (funcall disasm-op code-block index name mode)))))

(defun arg-formatter (arg mode)
  "Given an instruction's ARG, format it for display using the MODE's PRINTER."
  (if (member mode '(absolute absolute-x absolute-y indirect))
      (format nil (printer mode) (reverse arg))
      (format nil (printer mode) arg)))

(defun print-instruction (bytes index name mode)
  "Format the instruction at INDEX and its operands for display."
  (let ((byte-str (format nil "~{~2,'0x ~}" bytes))
        (args-str (format nil "~A ~A" name (arg-formatter (rest bytes) mode)))
        (docs (documentation name 'function)))
    (format t "$~4,'0x   ~9A  ;; ~14A ~A~%" index byte-str args-str docs)))

(defun sexpify-instruction (bytes index name mode)
  "Given BYTES and metadata, return a sexp-format representation of it."
  (declare (ignore index))
  (alexandria:if-let ((args (rest bytes))
                      (args-str (arg-formatter (rest bytes) mode)))
    (mapcar #'make-keyword (list name args-str))
    (mapcar #'make-keyword (list name))))

(defmacro with-disasm ((start end &key op) &body body)
  "Loop from START to END, passing each instruction to OP and execute BODY."
  `(loop with index = ,start while (< index ,end)
      for (step result) = (disasm-ins index ,@(when op (list op)))
        ,@body))

(defun disasm (start end)
  "Disassemble memory from START to END."
  (with-disasm (start end) do (incf index step)))

(defun disasm-to-str (start end)
  "Call DISASM with the provided args and return its output as a string."
  (with-output-to-string (*standard-output*)
    (disasm start end)))

(defun disasm-to-list (start end)
  "Disassemble a given region of memory into a sexp-based format."
  (with-disasm (start end :op #'sexpify-instruction)
    do (incf index step) collect result))
