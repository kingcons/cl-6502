(in-package :6502-cpu)

; TODO: Error gracefully or properly handle segments that cut off operands.
(defun disasm (start end &optional storage)
  "Disassemble memory from START to END. The byte at START is assumed to be an
instruction and not an operand. If STORAGE is non-nil, it is assumed to be a
bytevector to be diassembled in lieu of *ram*."
  (let* ((bytes (if storage
                    (subseq storage start end)
                    (get-range start end)))
         (bytes-length (length bytes))
         (index 0))
    (loop until (null index)
       do (let ((ins-length (disasm-instruction bytes index)))
            (if (>= (+ index ins-length) bytes-length)
                (setf index nil)
                (incf index ins-length))))))

(defun disasm-instruction (bytes index)
  "Lookup the metadata for the instruction at INDEX in BYTES, pass the info to
print-instruction for formatting and display, and return the instruction length."
  (let ((instruction (aref bytes index)))
    (destructuring-bind (name cycles length mode) (aref *opcodes* instruction)
      (declare (ignore cycles))
      (let ((docs (documentation name 'function)))
        (print-instruction (subseq bytes index (+ index length))
                           index name mode docs))
      length)))

(defun print-instruction (bytes index name mode docs)
  "Format the instruction at INDEX and its operands for display."
  (declare (ignore mode))
  (let* ((byte-str (format nil "~{~2,'0x ~}" (coerce bytes 'list)))
         (formatter "$~4,'0x   ~8A  ;; ~A,  ~A~%"))
    (format t formatter index byte-str name docs)))
