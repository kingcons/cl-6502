(in-package :6502-cpu)

;;;; Abstract Conditions

(define-condition 6502-error (error)
  ()
  (:documentation "The base condition for all errors in 6502."))

;;;; Concrete Conditions

(define-condition status-bit-error (6502-error)
  ((index :initarg :index :reader index))
  (:report (lambda (condition stream)
             (format stream "Tried to set status bit ~D to a non-bit value."
                     (index condition))))
  (:documentation "A bit can only be zero or one."))

(define-condition illegal-opcode (6502-error)
  ((opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "~A is not a legal opcode." (opcode condition))))
  (:documentation "Illegal opcodes are not currently implemented."))

(define-condition not-implemented-yet (6502-error)
  ((opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "Opcode ~A (~A) has not been implemented yet."
                     (opcode condition) (aref *opcodes* (opcode condition)))))
  (:documentation "Some opcodes have not yet been implemented."))
