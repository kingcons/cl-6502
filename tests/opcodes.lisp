(in-package :6502-tests)

(def-suite :opcodes)
(in-suite :opcodes)

(deftest brk-sets-flags
  "brk flag (4) and disable interrupts flag (2) must be set."
  (with-fixture cpu ()
    (6502:brk #x00)
    (is (logbitp 4 (cpu-sr *cpu*)))
    (is (logbitp 2 (cpu-sr *cpu*)))))

(deftest brk-adds-3-bytes-to-stack
  "Program Counter (2) + Stack Pointer (1) == 3 bytes
   The stack is decremented from #xFF giving #xFC."
  (with-fixture cpu ()
    (6502:brk #x00)
    (is (= (cpu-sp *cpu*) #xfc))))

(deftest ora-sets-zero-flag
  "Zero flag must be set after calling ORA."
  (with-fixture cpu ()
    (6502:ora #x05)
    (is (logbitp 1 (cpu-sr *cpu*)))))
