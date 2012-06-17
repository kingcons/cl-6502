(in-package :6502)

(defgeneric execute (cpu &optional program)
  (:documentation "Step the CPU until a BRK occurs. If PROGRAM is provided,
it will be placed at the beginning of *ram* and the PC will be set to 0.
PROGRAM should be a list or array of bytes.")
  (:method ((cpu cpu) &optional program)
    (when program
      (setf (get-range 0) program
            (cpu-pc cpu) 0))
    (loop for result = (6502-step cpu (zero-page cpu))
       until (eql :done result)
       finally (print result))
    cpu))

(defgeneric 6502-step (cpu opcode)
  (:documentation "Step the CPU through the next instruction.")
  (:method ((cpu cpu) opcode)
    (when (zerop opcode)
      (return-from 6502-step :done))
    (setf (cpu-pc cpu) (wrap-word (1+ (cpu-pc cpu))))
    (handler-case (funcall (get-instruction opcode) opcode)
      (undefined-function ()
        (error 'illegal-opcode :opcode opcode)))))
