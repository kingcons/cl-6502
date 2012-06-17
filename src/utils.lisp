(in-package :6502)

(defgeneric execute (cpu)
  (:documentation "Step the CPU until a BRK occurs.")
  (:method ((cpu cpu))
    (loop for result = (next)
       until (eql :done result)
       finally (print result))
    cpu))

(defun next (&optional start (cpu *cpu*))
  "A simple wrapper for 6502-step. START behaves as in execute."
  (when start
    (setf (cpu-pc cpu) start))
  (6502-step cpu (zero-page cpu)))

(defgeneric 6502-step (cpu opcode)
  (:documentation "Step the CPU through the next instruction.")
  (:method ((cpu cpu) opcode)
    (when (zerop opcode)
      (return-from 6502-step :done))
    (setf (cpu-pc cpu) (wrap-word (1+ (cpu-pc cpu))))
    (handler-case (funcall (get-instruction opcode) opcode)
      (undefined-function ()
        (error 'illegal-opcode :opcode opcode)))))
