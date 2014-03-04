(in-package :6502-tests)

(def-suite opcodes :in 6502-tests)
(in-suite opcodes)

(defun klaus-init (&optional (file "tests/test.bin"))
  (let ((test-rom (read-file-into-byte-vector (app-path file))))
    (setf (get-range #x0a) test-rom
          (cpu-pc *cpu*) #x1000)))

(defun klaus-test ()
  (let ((cycles (* 78 (expt 2 20))))
    (loop until (> (cpu-cc *cpu*) cycles)
       for opcode = (get-byte (cpu-pc *cpu*))
       do (step-cpu *cpu* opcode))))

(defun speedrun ()
  (klaus-init "tests/test-brk.bin")
  (time (6502::execute *cpu*)))

(deftest pass-klaus-test-suite
    "We should pass Klaus Dorfmann's test suite."
  (klaus-init)
  (klaus-test)
  (destructuring-bind (op addr) (current-instruction cpu)
    (let* ((addr-text (symbol-name addr))
           (hex-num (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" addr-text))
           (pc (parse-integer hex-num :radix 16)))
      (is (and (eql op :jmp) (eql pc (cpu-pc cpu))))
      ;; There are multiple traps in the code that are jump-to-self.
      ;; Only 0x3c37 is the 'success' macro. Disasm surrounding code to verify.
      (is (eql (cpu-pc cpu) #x3c37)))))
