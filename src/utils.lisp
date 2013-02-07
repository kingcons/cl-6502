(in-package :6502)

(defgeneric execute (cpu &optional program)
  (:documentation "Step the CPU until a BRK. If a PROGRAM bytevector is supplied,
it will be placed at the beginning of *ram* and the PC will be set to 0.")
  (:method ((cpu cpu) &optional program)
    (when program
      (setf (get-range 0) program))
    (loop for result = (6502-step cpu (get-byte (immediate cpu)))
       until (eql :done result)
       finally (print result))
    cpu))

(defgeneric 6502-step (cpu opcode)
  (:documentation "Step the CPU through the next instruction, returning the CPU
or :done.")
  (:method ((cpu cpu) opcode)
    (setf (cpu-pc cpu) (wrap-word (1+ (cpu-pc cpu))))
    (handler-case
        (let ((result (funcall (get-instruction opcode) opcode)))
          (if (zerop opcode)
              :done
              result))
      (undefined-function ()
        (error 'illegal-opcode :opcode opcode)))))

(defun load-binary (path)
  "Load the binary file at PATH into memory."
  (let ((binary (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((seq (make-array (file-length in))))
                    (read-sequence seq in)
                    seq))))
    (setf (get-range (- #x10000 (length binary))) binary)))
