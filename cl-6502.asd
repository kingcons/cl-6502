(defsystem #:cl-6502
  :name "cl-6502"
  :description "An emulator for the MOS 6502 CPU"
  :version "0.9.8-dev"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:alexandria :cl-ppcre)
  :serial t
  :components ((:file "packages")
               (:file "conditions")
               (:file "addressing")
               (:file "cpu")
               (:file "disassemble")
               (:file "parser")
               (:file "assemble")
               (:file "opcodes")
               (:file "jit")
               (:file "utils"))
  :in-order-to ((test-op (test-op cl-6502-test))))

(defsystem #:cl-6502-test
  :description "A test suite for cl-6502."
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :depends-on (:cl-6502 :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "fixtures")
               (:file "assembler")
               (:file "disassembler")
               (:file "parser")
               (:file "opcodes")
               (:file "jit")
               #+sbcl (:file "perf"))
  :perform (test-op :after (op c)
                    (uiop:symbol-call :6502-tests 'run!)))

(defpackage #:6502-conf (:export #:app-path))
(defvar 6502-conf::*basedir*
  (make-pathname :defaults *load-truename* :name nil :type nil))
(defun 6502-conf:app-path (path &rest args)
  (merge-pathnames (apply 'format nil path args) 6502-conf::*basedir*))
