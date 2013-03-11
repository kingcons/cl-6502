(in-package :6502-tests)

(def-suite opcodes :in 6502-tests)
(in-suite opcodes)

;; Klaus test-suite jumps to self to indicate successful completion.
(defvar *debug* nil)

(defmethod 6502-step :before ((cpu cpu) opcode)
  (when *debug*
    (6502::disasm-ins (immediate cpu))))

(defmethod 6502::jmp :around ((opcode (eql 76)) cpu &key mode setf-form)
  (when *debug*
    (let ((result (call-next-method)))
      (if (= (absolute cpu) (cpu-pc cpu))
          :done
          result))))

(defun klaus-init ()
  (let ((test-rom (read-file-into-byte-vector (app-path "tests/test.bin"))))
    (setf (get-range #x0a) test-rom
          (cpu-pc *cpu*) #x1000)))

(deftest pass-klaus-test-suite
    "We should pass Klaus Dorfmann's test suite."
  (klaus-init)
  (let ((*debug* t))
    (is (eql (execute cpu) :done))))
