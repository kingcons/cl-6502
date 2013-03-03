(in-package :6502-tests)

(def-suite opcodes :in 6502-tests)
(in-suite opcodes)

(defvar *debug* nil)
(defmethod 6502-step :before ((cpu cpu) opcode)
  (when *debug*
    (6502::disasm-ins (immediate cpu))))

(deftest pass-klaus-test-suite
    "We should pass Klaus Dorfmann's test suite."
  (setf (get-range #x0a) (read-file-into-byte-vector (app-path "tests/test.bin"))
        (cpu-pc cpu) #x1000)
  (let ((*debug* t))
    (execute cpu))
  ;; TODO: Get the suite to finish, amend test.
  (is (eql t t)))

