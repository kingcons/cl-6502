(in-package :6502-tests)

(def-suite disassembler :in 6502-tests)
(in-suite disassembler)

(deftest disassemble-implied
    "Implied mode instructions should disassemble correctly."
  (setf (get-byte 0) 0)
  (is (search "BRK" (disasm-to-str 0 1)))
  (setf (get-byte 0) 234)
  (is (search "NOP" (disasm-to-str 0 1))))
