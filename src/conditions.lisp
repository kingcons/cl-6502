(in-package :6502-cpu)

;;;; Abstract Conditions

(define-condition 6502-error (error)
  ()
  (:documentation "The base condition for all errors in 6502."))

;;;; Concrete Conditions

(define-condition illegal-opcode (6502-error)
  ((opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "~A is not a legal opcode." (opcode condition))))
  (:documentation "Illegal opcodes are not currently implemented."))

(define-condition invalid-syntax (6502-error)
  ((line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (format stream "Syntax for line ~S is invalid." (line condition))))
  (:documentation "Assembly must conform to the syntax in the README."))

(define-condition invalid-mode (6502-error)
  ((mode :initarg :mode :reader mode))
  (:report (lambda (condition stream)
             (format stream "~S is not a valid addressing mode."
                     (mode condition))))
  (:documentation "Only the 6502 addressing modes have readers and printers."))
