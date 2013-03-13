(in-package :6502-tests)

(def-suite disassembler :in 6502-tests)
(in-suite disassembler)

(deftest disassemble-implied
    "Implied mode instructions should disassemble correctly."
  (setf (get-byte 0) 0)
  (is (search "BRK" (disasm-to-str 0 1)))
  (setf (get-byte 0) 234)
  (is (search "NOP" (disasm-to-str 0 1))))

(deftest disasm-to-list
    "We should be able to disassemble code to a sexp-based format."
  (setf (get-byte 0) 0)
  (is (equalp (6502::disasm-to-list 0 1) '((:brk))))
  (setf (get-byte 0) 234)
  (is (equalp (6502::disasm-to-list 0 1) '((:nop)))))

(deftest disasm-to-list-with-args
    "We should be able to disassemble code with args in a sexp-based format."
  (setf (get-range 0) #(160 0 200 208 253 237 1 0))
  (is (equalp (6502::disasm-to-list 0 7)
              '((:ldy :#$00) (:iny) (:bne :&fd) (:sbc :$0001)))))

(deftest current-instruction
    "We should be able to inspect the current instruction."
  (setf (get-range #x3fa5) #(76 165 63)
        (cpu-pc cpu) #x3fa5)
  (is (equalp (6502::current-instruction cpu) '(:jmp :$3fa5))))
