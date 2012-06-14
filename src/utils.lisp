(in-package :6502)

(defmethod execute ((cpu cpu))
  "Step the CPU until a BRK occurs."
  (loop for result = (next)
        until (eql :done result)
        finally (print result))
  cpu)

(defun next (&optional start (cpu *cpu*))
  "A simple wrapper for 6502-step. START behaves as in execute."
  (when start
    (setf (cpu-pc cpu) start))
  (6502-step cpu (zero-page cpu)))

(defmethod 6502-step ((cpu cpu) opcode)
  "Step the CPU through the next instruction."
  (when (zerop opcode)
    (return-from 6502-step :done))
  (setf (cpu-pc cpu) (wrap-word (1+ (cpu-pc cpu))))
  (handler-case (funcall (get-instruction opcode) opcode)
    (undefined-function ()
      (error 'illegal-opcode :opcode opcode))
    ;; KLUDGE: Catch simple-error for now as there isn't a
    ;; no-applicable-method condition. A suggestion from pjb:
    ;; (defmethod no-applicable-method ((fun t) &rest args) (error ...))
    (simple-error ()
      (error 'not-yet-implemented :opcode opcode))))
