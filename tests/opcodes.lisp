(in-package :6502-tests)

(def-suite opcodes :in 6502-tests)
(in-suite opcodes)

(deftest brk-sets-flags
  "brk flag (4) and disable interrupts flag (2) must be set."
  (6502::brk #x00 cpu)
  (is (plusp (status-bit :break cpu)))
  (is (plusp (status-bit :interrupt cpu))))

(deftest brk-adds-3-bytes-to-stack
  "Program Counter (2) + Stack Pointer (1) == 3 bytes
   The stack is decremented from #xFF giving #xFC."
  (6502::brk #x00 cpu)
  (is (= (cpu-sp cpu) #xfc)))

