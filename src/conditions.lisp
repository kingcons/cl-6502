(in-package :6502)

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
