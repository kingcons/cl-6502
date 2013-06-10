(in-package :6502)

(defgeneric execute (cpu)
  (:documentation "Step the CPU until a BRK.")
  (:method ((cpu cpu))
    (loop for result = (6502-step cpu (getter 'immediate nil cpu))
       until (eql :done result)
       finally (print result))
    cpu))

(defgeneric 6502-step (cpu opcode)
  (:documentation "Step the CPU through the next instruction, returning the CPU
or :done.")
  (:method ((cpu cpu) opcode)
    (handler-case
        (destructuring-bind (name &rest args) (aref *opcodes* opcode)
          (let ((result (apply name cpu args)))
            (if (zerop opcode)
                :done
                result)))
      (undefined-function ()
        (error 'illegal-opcode :opcode opcode)))))
