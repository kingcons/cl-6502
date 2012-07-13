(in-package :6502-tests)

(def-suite assembler :in 6502-tests)
(in-suite assembler)

(deftest assemble-ignores-case
    "Case shouldn't come into play in assembly."
  (is (eql (asm "brk") #(0)))
  (is (eql (asm "BRK") #(0)))
  (is (eql (asm "bRK") #(0)))
  (is (eql (asm "LSR a") #(#x4a)))
  (is (eql (asm "lsr A") #(#x4a))))

(deftest assemble-implied
    "Implied mode instructions should be assembled correctly."
  (is (eql (asm "nop") #(#xea))))

(deftest assemble-accumulator
    "Accumulator mode instructions should be assembled correctly."
  (is (eql (asm "rol a") #(#x2a))))

(deftest assemble-immediate
    "Immediate mode instructions should be assembled correctly."
  (is (eql (asm "lda #$00") #(#xa9 0))))

(deftest assemble-zero-page
    "Zero-page mode instructions should be assembled correctly."
  (is (eql (asm "lda $03") #(#xa5 3))))

(deftest assemble-zero-page-x
    "Zero-page-x mode instructions should be assembled correctly."
  (is (eql (asm "lda $03, x") #(#xb5 3))))

(deftest assemble-zero-page-y
    "Zero-page-y mode instructions should be assembled correctly."
  (is (eql (asm "ldx $03, y") #(#xb6 3))))

(deftest assemble-absolute
    "Absolute mode instructions should be assembled correctly."
  (is (eql (asm "sbc #$0001") #(#xed 01 00))))

(deftest assemble-absolute-x
    "Absolute-x mode instructions should be assembled correctly."
  (is (eql (asm "lda $1234, x") #(#xbd #x34 #x12))))

(deftest assemble-absolute-y
    "Absolute-y mode instructions should be assembled correctly."
  (is (eql (asm "lda $1234, y") #(#xb9 #x34 #x12))))

(deftest assemble-indirect
    "Indirect mode instructions should be assembled correctly."
  (is (eql (asm "jmp ($1234)") #(#x6c #x34 #x12))))

(deftest assemble-indirect-x
    "Indirect-x mode instructions should be assembled correctly."
  (is (eql (asm "lda ($12), x") #(#xa1 #x12))))

(deftest assemble-indirect-y
    "Indirect-y mode instructions should be assembled correctly."
  (is (eql (asm "lda ($34), y") #(#xb1 #x34))))

(deftest assemble-relative
    "Relative mode instructions should be assembled correctly."
  (is (eql (asm "bne $fd") #(#xd0 #xfd))))

(deftest assemble-comment
    "Comments (;) should be ignored. Code before comments should not be ignored."
  (is (eql (asm "  ; blah blah blah") #()))
  (is (eql (asm "  BRK ; foo bar baz") #(0))))

(deftest assemble-label
    "Labels ($LABEL:) should store a reference to the current PC."
  (let ((code (format nil " loop: LDY #$00~% INY~% jmp loop")))
    (is (eql (asm code) #(#xa0 0 #xc8 0)))))

(deftest assemble-data
    "Variables ($VAR=value) should store values."
  (let ((code (format nil " hours=12~% lda hours~% hours=24~% ldy hours")))
    (is (eql (asm code) #(#xa9 12 #xa0 24)))))

; (deftest assemble-pc "*" nil)?

(deftest assemble-program
    "A basic program should assemble correctly."
  nil)
