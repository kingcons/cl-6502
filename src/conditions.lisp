(in-package :6502-cpu)

;;;; Abstract Conditions

(define-condition 6502-error (error)
  ()
  (:documentation "The base condition for all errors in 6502."))

;;;; Concrete Conditions

(define-condition status-bit-error (6502-error)
  ((index :initarg :index :reader index))
  (:report (lambda (condition stream)
             (format stream "Cannot set status-bit ~D to a non-bit value."
                     (index condition))))
  (:documentation "An invalid status-register value was provided."))

(define-condition illegal-opcode (6502-error)
  ((opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "~A is not a legal opcode." (opcode condition))))
  (:documentation "Illegal opcodes are not currently implemented."))

(define-condition unknown-mode (6502-error)
  ((tokens :initarg :tokens :reader tokens))
  (:report (lambda (condition stream)
             (format stream "Couldn't find mode matching: ~A."
                     (tokens condition))))
  (:documentation "Assembly must conform to one of the 13 addressing modes
supported by cl-6502."))

(define-condition syntax-error (6502-error)
  ((token :initarg :token :reader token)
   (line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (format stream "Couldn't parse token ~A on line ~A."
                     (token condition) (line condition))))
  (:documentation "Failure to parse a given token or label."))
