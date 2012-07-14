(in-package :6502-tests)

(def-suite assembler :in 6502-tests)
(in-suite assembler)

(deftest assemble-ignores-case
    "Case shouldn't come into play in assembly."
  (is (equalp (asm "brk") #(0)))
  (is (equalp (asm "BRK") #(0)))
  (is (equalp (asm "bRK") #(0)))
  (is (equalp (asm "LSR a") #(#x4a)))
  (is (equalp (asm "lsr A") #(#x4a))))

(deftest assemble-implied
    "Implied mode instructions should be assembled correctly."
  (is (equalp (asm "nop") #(#xea))))

(deftest assemble-accumulator
    "Accumulator mode instructions should be assembled correctly."
  (is (equalp (asm "rol a") #(#x2a))))

(deftest assemble-immediate
    "Immediate mode instructions should be assembled correctly."
  (is (equalp (asm "lda #$00") #(#xa9 0))))

(deftest assemble-zero-page
    "Zero-page mode instructions should be assembled correctly."
  (is (equalp (asm "lda $03") #(#xa5 3))))

(deftest assemble-zero-page-x
    "Zero-page-x mode instructions should be assembled correctly."
  (is (equalp (asm "lda $03, x") #(#xb5 3))))

(deftest assemble-zero-page-y
    "Zero-page-y mode instructions should be assembled correctly."
  (is (equalp (asm "ldx $03, y") #(#xb6 3))))

(deftest assemble-absolute
    "Absolute mode instructions should be assembled correctly."
  (is (equalp (asm "sbc $0001") #(#xed 01 00))))

(deftest assemble-absolute-x
    "Absolute-x mode instructions should be assembled correctly."
  (is (equalp (asm "lda $1234, x") #(#xbd #x34 #x12))))

(deftest assemble-absolute-y
    "Absolute-y mode instructions should be assembled correctly."
  (is (equalp (asm "lda $1234, y") #(#xb9 #x34 #x12))))

(deftest assemble-indirect
    "Indirect mode instructions should be assembled correctly."
  (is (equalp (asm "jmp ($1234)") #(#x6c #x34 #x12))))

(deftest assemble-indirect-x
    "Indirect-x mode instructions should be assembled correctly."
  (is (equalp (asm "lda ($12), x") #(#xa1 #x12))))

(deftest assemble-indirect-y
    "Indirect-y mode instructions should be assembled correctly."
  (is (equalp (asm "lda ($34), y") #(#xb1 #x34))))

(deftest assemble-relative
    "Relative mode instructions should be assembled correctly."
  (is (equalp (asm "bne &fd") #(#xd0 #xfd))))

(deftest assemble-comment
    "Comments (;) should be ignored. Code before comments should not be ignored."
  (is (equalp (asm "  ; blah blah blah") #()))
  (is (equalp (asm "  BRK ; foo bar baz") #(0))))

(deftest assemble-label
    "Labels ($LABEL:) should store a reference to the current PC."
  (let ((code (format nil " loop: ~% LDY #$00~% INY~% jmp $loop")))
    (is (equalp (asm code) #(#xa0 0 #xc8 #x4c 0 0)))))

(deftest assemble-data
    "Variables ($VAR=value) should store values."
  (let ((code (format nil " hours=12~% lda #$hours~% hours=24~% ldy #$hours")))
    (is (equalp (asm code) #(#xa9 18 #xa0 36)))))

; (deftest assemble-pc "*" nil)?

(deftest assemble-program
    "A basic program should assemble correctly."
  nil)
