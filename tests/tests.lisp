(defpackage :6502-tests
  (:use :cl :6502 :fiveam)
  (:export #:run!))

(in-package :6502-tests)

(reset) ; Initialize the CPU.

;; Thanks to Michael Weber for a rough blueprint of shallow instance copying.
(defmacro with-cpu (&body body)
  "Store a copy of *CPU* and execute BODY in an unwind-protect which restores
the old value when BODY finishes."
  (alexandria:with-gensyms (backup)
    `(let ((,backup (copy-cpu *cpu*)))
       (unwind-protect (progn ,@body)
         (setf *cpu* ,backup)))))

(def-suite :opcodes)
(in-suite :opcodes)

(test brk-sets-flags
  ;; brk flag (4) and disable interrupts flag (2) must be set.
  (with-cpu
    (brk #x00)
    (is (logbitp 4 (cpu-sr *cpu*)))
    (is (logbitp 2 (cpu-sr *cpu*)))))

(test brk-adds-24bits-to-stack
  ;; Program Counter (16) + Stack Pointer (8) == 24 bits
  ;; The stack is decremented from #xFF giving #xFC.
  (with-cpu
    (brk #x00)
    (is (= (cpu-sp *cpu*) #xfc))))
