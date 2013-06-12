(in-package :6502-tests)

(def-suite opcodes :in 6502-tests)
(in-suite opcodes)

(defun klaus-init ()
  (let ((test-rom (read-file-into-byte-vector (app-path "tests/test.bin"))))
    (setf (get-range #x0a) test-rom
          (cpu-pc *cpu*) #x1000)))

(defun klaus-test ()
  (let ((cycles (* 45 (expt 2 21))))
    (loop until (> (cpu-cc *cpu*) cycles)
       for opcode = (get-byte (cpu-pc *cpu*))
       do (step-cpu *cpu* opcode))))

(deftest pass-klaus-test-suite
    "We should pass Klaus Dorfmann's test suite."
  (klaus-init)
  (klaus-test)
  (destructuring-bind (op addr) (current-instruction cpu)
    (is (and (eql op :jmp)
             (eql (6502::extract-num (format nil "~A" addr)) (cpu-pc cpu))))
    ;; There are multiple traps in the code that are jump-to-self.
    ;; Only 0x3c37 is the 'success' macro. Disasm surrounding code to verify.
    (is (eql (cpu-pc cpu) #x3c37))))
