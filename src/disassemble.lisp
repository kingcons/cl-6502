(in-package :6502-cpu)

(defun disasm (start end)
  "Disassemble memory from START to END."
  (loop with index = start while (< index end)
     do (incf index (disasm-ins index))))

(defun disasm-ins (index)
    "Lookup the metadata for the instruction at INDEX and pass it to
print-instruction for formatting and display, returning the instruction length."
  (destructuring-bind (name cycles length mode) (aref *opcodes* (get-byte index))
    (declare (ignore cycles))
    (let ((code-block (coerce (get-range index (+ index length)) 'list)))
      (print-instruction code-block index name mode)
      length)))

(defun print-instruction (bytes index name mode)
  "Format the instruction at INDEX and its operands for display."
  (flet ((arg-formatter (arg)
           (if (member mode '(absolute absolute-x absolute-y indirect))
               (format nil (printer mode) (reverse arg))
               (format nil (printer mode) arg))))
    (let ((byte-str (format nil "~{~2,'0x ~}" bytes))
          (args-str (format nil "~A ~A" name (arg-formatter (subseq bytes 1))))
          (docs (documentation name 'function)))
      (format t "$~4,'0x   ~9A  ;; ~14A ~A~%" index byte-str args-str docs))))

(defun disasm-to-str (start end)
  "Call DISASM with the provided args and return its output as a string."
  (with-output-to-string (*standard-output*)
    (disasm start end)))
