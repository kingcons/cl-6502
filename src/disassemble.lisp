(in-package :6502)

(defmacro with-disasm ((start end &key op) &body body)
  "Loop from START to END, passing each instruction to OP and execute BODY.
OP is PRINT-INSTRUCTION by default. Within BODY, the return value of OP is
bound to RESULT and the length of the instruction in bytes is bound to STEP."
  `(loop with index = ,start while (<= index ,end)
      for (step result) = (disasm-ins index ,@(when op (list op)))
      do (incf index step) ,@body))

(defun disasm (start end)
  "Disassemble memory from START to END."
  (with-disasm (start end)))

(defun disasm-to-list (start end)
  "Disassemble a given region of memory into a sexp-based format."
  (with-disasm (start end :op #'sexpify-instruction) collect result))

(defun disasm-to-str (start end)
  "Call DISASM with the provided args and return its output as a string."
  (with-output-to-string (*standard-output*) (disasm start end)))

(defun disasm-ins (index &optional (disasm-op #'print-instruction))
    "Lookup the metadata for the instruction at INDEX and pass it to
DISASM-OP for formatting and display, returning the instruction length."
  (destructuring-bind (name docs cycles bytes mode)
      (aref *opcode-meta* (get-byte index))
    (declare (ignore cycles))
    (let ((code-block (coerce (get-range index (+ index bytes)) 'list)))
      (list bytes (funcall disasm-op code-block index name docs mode)))))

(defun print-instruction (bytes index name docs mode)
  "Format the instruction at INDEX and its operands for display."
  (let ((byte-str (format nil "~{~2,'0x ~}" bytes))
        (args-str (format nil "~A ~A" name (arg-formatter (rest bytes) mode))))
    (format t "$~4,'0x   ~9A  ;; ~14A ~A~%" index byte-str args-str docs)))

(defun sexpify-instruction (bytes index name docs mode)
  "Given BYTES and metadata, return a sexp-format representation of it."
  (declare (ignore index docs))
  (alexandria:if-let ((args (rest bytes))
                      (args-str (bytes-to-keyword-syntax bytes mode)))
    (mapcar #'make-keyword (list name args-str))
    (mapcar #'make-keyword (list name))))

(defun arg-formatter (arg mode)
  "Given an instruction's ARG, format it for display using the MODE's WRITER."
  (if (member mode '(absolute absolute-x absolute-y indirect))
      (format nil (writer mode) (reverse arg))
      (format nil (writer mode) arg)))

(defun bytes-to-keyword-syntax (bytes mode)
  "Take BYTES and a MODE and return our assembly representation of the arguments."
  (let ((result (arg-formatter (rest bytes) mode)))
    (flet ((munge-indirect (str)
             (cl-ppcre:regex-replace "\\(\\$(.*)\\)(.*)?" str "@\\1\\2")))
      (cl-ppcre:regex-replace ", " (munge-indirect result) "."))))

(defun current-instruction (cpu &optional print-p)
  "Return a list representing the current instruction. If PRINT-P is non-nil,
print the current address and instruction and return NIL."
  (let ((fn (if print-p #'print-instruction #'sexpify-instruction)))
    (second (disasm-ins (cpu-pc cpu) fn))))
