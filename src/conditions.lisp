(in-package :6502)

(define-condition illegal-opcode ()
  ((opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "~X is not a legal opcode." (opcode condition))))
  (:documentation "Illegal opcodes are not currently implemented."))

(define-condition invalid-syntax ()
  ((line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (format stream "Syntax for line ~S is invalid." (line condition))))
  (:documentation "Assembly must conform to the syntax in the README."))

(define-condition invalid-mode ()
  ((mode :initarg :mode :reader mode))
  (:report (lambda (condition stream)
             (format stream "~A is not a valid addressing mode." (mode condition))))
  (:documentation "Only the 6502 addressing modes have readers and printers."))
