(in-package :6502)

(declaim (inline step-cpu))
(defun step-cpu (cpu opcode)
  "Step the CPU through the next OPCODE."
  #f
  (funcall (the function (aref *opcode-funs* opcode)) cpu))

(defun execute (cpu)
  "Step the CPU until a BRK instruction."
  #f
  (declare (inline get-byte))
  (loop for opcode of-type u8 = (get-byte (cpu-pc cpu))
     do (step-cpu cpu opcode)
     until (zerop opcode)))
