(defsystem #:cl-6502
  :name "cl-6502"
  :description "An emulator for the MOS 6502 CPU"
  :version "0.0.1"
  :license "LLGPL"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:alexandria :closer-mop)
  :serial t
  :components ((:file "packages")
               (:file "cpu")
               (:file "opcodes"))
  :in-order-to ((test-op (load-op cl-6502-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :6502-tests))))

(defsystem #:cl-6502-tests
  :depends-on (cl-6502 fiveam)
  :pathname "tests/"
  :components ((:file "tests")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :cl-6502))))
  (values nil))
