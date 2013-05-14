(defsystem #:cl-6502
  :name "cl-6502"
  :description "An emulator for the MOS 6502 CPU"
  :version "0.9.2"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:alexandria :cl-ppcre)
  :serial t
  :components ((:file "packages")
               (:file "conditions")
               (:file "cpu")
               (:file "addressing")
               (:file "disassemble")
               (:file "assemble")
               (:file "opcodes")
               (:file "jit")
               (:file "utils"))
  :in-order-to ((test-op (load-op cl-6502-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :6502-tests)
                             (intern "6502-TESTS" :6502-tests))))

(defsystem #:cl-6502-tests
  :depends-on (cl-6502 fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "fixtures")
               (:file "assembler")
               (:file "disassembler")
               (:file "addressing")
               (:file "opcodes")
               (:file "jit")
               #+sbcl (:file "perf")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :cl-6502))))
  (values nil))

(defpackage #:6502-conf (:export #:app-path))
(defvar 6502-conf::*basedir*
  (make-pathname :defaults *load-truename* :name nil :type nil))
(defun 6502-conf:app-path (path &rest args)
  (merge-pathnames (apply 'format nil path args) 6502-conf::*basedir*))
