(in-package :6502)

(defun execute (cpu)
  "Step the CPU until a BRK instruction."
  (loop for opcode = (get-byte (cpu-pc cpu))
     do (handler-case (step-cpu cpu opcode)
          (undefined-function ()
            (error 'illegal-opcode :opcode opcode)))
     until (zerop opcode)))

(defun step-cpu (cpu opcode)
  "Step the CPU through the next OPCODE."
  (destructuring-bind (name &rest args) (aref *opcodes* opcode)
    (apply name cpu args)))
