(in-package :6502-cpu)

; TODO: Error gracefully or properly handle segments that cut off operands.
(defun disasm (start end &optional storage)
  "Disassemble memory from START to END. The byte at START is assumed to be an
instruction and not an operand. If STORAGE is non-nil, it is assumed to be a
bytevector to be diassembled in lieu of *ram*."
  (let ((bytes (if storage
                   (subseq storage start end)
                   (get-range start end)))
         (index 0))
    (assert (typep bytes 'vector))
    (loop do (incf index (disasm-instruction bytes index))
       until (>= index (length bytes)))))

(defun disasm-instruction (bytes index)
  "Lookup the metadata for the instruction at INDEX in BYTES, pass the info to
print-instruction for formatting and display, and return the instruction length."
  (destructuring-bind (name cycles length mode) (aref *opcodes* (aref bytes index))
    (declare (ignore cycles))
    (let ((code-block (coerce (subseq bytes index (+ index length)) 'list)))
      (print-instruction code-block index name mode))
    length))

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

(defun disasm-to-str (start end &optional bytes)
  "Call DISASM with the provided args and return its output as a string."
  (with-output-to-string (*standard-output*)
    (disasm start end bytes)))
