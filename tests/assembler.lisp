(in-package :6502-tests)

(def-suite assembler :in 6502-tests)
(in-suite assembler)

(defmacro bvec (&rest args)
  "Construct a byte vector from ARGS where ARGS are symbols representing hex."
  (let ((nums (cl-ppcre:split " " (format nil "~{~A~^ ~}" args))))
    `(vector ,@(mapcar (lambda (x) (parse-integer x :radix 16)) nums))))

(deftest assemble-ignores-case
    "Case shouldn't come into play in assembly."
  (is (equalp (asm "brk") #(0)))
  (is (equalp (asm "BRK") #(0)))
  (is (equalp (asm "bRK") #(0)))
  (is (equalp (asm "LSR a") #(#x4a)))
  (is (equalp (asm "lsr A") #(#x4a))))

(deftest assemble-implied
    "Implied mode instructions should be assembled correctly."
  (is (equalp (asm "nop") #(#xea)))
  (is (equalp (asm '((:nop))) #(#xea))))

(deftest assemble-accumulator
    "Accumulator mode instructions should be assembled correctly."
  (is (equalp (asm "rol a") #(#x2a)))
  (is (equalp (asm '((:rol :a))) #(#x2a))))

(deftest assemble-immediate
    "Immediate mode instructions should be assembled correctly."
  (let ((expected (bvec a9 00)))
    (is (equalp (asm "lda #$00") expected))
    (is (equalp (asm '((:lda :#$00))) expected))))

(deftest assemble-zero-page
    "Zero-page mode instructions should be assembled correctly."
  (let ((expected (bvec a5 03)))
    (is (equalp (asm "lda $03") expected))
    (is (equalp (asm '((:lda :$03))) expected))))

(deftest assemble-zero-page-x
    "Zero-page-x mode instructions should be assembled correctly."
  (let ((expected (bvec b5 03)))
    (is (equalp (asm "lda $03, x") expected))
    (is (equalp (asm '((:lda :$03.x))) expected))))

(deftest assemble-zero-page-y
    "Zero-page-y mode instructions should be assembled correctly."
  (let ((expected (bvec b6 03)))
    (is (equalp (asm "ldx $03, y") expected))
    (is (equalp (asm '((:ldx :$03.y))) expected))))

(deftest assemble-absolute
    "Absolute mode instructions should be assembled correctly."
  (let ((expected (bvec ed 1 0)))
    (is (equalp (asm "sbc $0001") expected))
    (is (equalp (asm '((:sbc :$0001))) expected))))

(deftest assemble-absolute-x
    "Absolute-x mode instructions should be assembled correctly."
  (let ((expected (bvec bd 34 12)))
    (is (equalp (asm "lda $1234, x") expected))
    (is (equalp (asm '((:lda :$1234.x))) expected))))

(deftest assemble-absolute-y
    "Absolute-y mode instructions should be assembled correctly."
  (let ((expected (bvec b9 34 12)))
    (is (equalp (asm "lda $1234, y") expected))
    (is (equalp (asm '((:lda :$1234.y))) expected))))

(deftest assemble-indirect
    "Indirect mode instructions should be assembled correctly."
  (let ((expected (bvec 6c 34 12)))
    (is (equalp (asm "jmp ($1234)") expected))
    (is (equalp (asm '((:jmp :@1234))) expected))))

(deftest assemble-indirect-x
    "Indirect-x mode instructions should be assembled correctly."
  (let ((expected (bvec a1 12)))
    (is (equalp (asm "lda ($12), x") expected))
    (is (equalp (asm '((:lda :@12.x))) expected))))

(deftest assemble-indirect-y
    "Indirect-y mode instructions should be assembled correctly."
  (let ((expected (bvec b1 34)))
    (is (equalp (asm "lda ($34), y") expected))
    (is (equalp (asm '((:lda :@34.y))) expected))))

(deftest assemble-relative
    "Relative mode instructions should be assembled correctly."
  (let ((expected (bvec d0 fd)))
    (is (equalp (asm "bne &fd") expected))
    (is (equalp (asm '((:bne :&fd))) expected))))

(deftest assemble-comment
    "Comments (;) should be ignored. Code before comments should not be ignored."
  (is (equalp (asm "  ; blah blah blah") #()))
  (is (equalp (asm "  BRK ; foo bar baz") #(0))))

(deftest assemble-label
    "Labels ($LABEL:) should store a reference to the current PC."
  (let ((code (format nil " loop: ~% LDY #$00~% INY~% jmp $loop")))
    (is (equalp (asm code) (bvec a0 00 c8 4c 00 00)))))

(deftest assemble-data
    "Variables ($VAR=value) should store values."
  (let ((code (format nil " std_hours=12~% lda #$std_hours~%
                            mil-hours=24~% ldy #$mil-hours")))
    (is (equalp (asm code) #(#xa9 18 #xa0 36)))))

(deftest assemble-program
    "A basic program should assemble correctly."
  (let ((code (format nil "CLC~% LDA #$00~% LDY #$00~%
                           loop:~% INY~% bne &loop~% sbc $0001~% brk")))
    (is (equalp (asm code) #(24 169 0 160 0 200 208 253 237 1 0 0)))
    (cl-6502:execute cpu (asm code))
    (is (eql (cpu-ar cpu) 86))))

(deftest assemble-forward-relative
    "A program with a forward relative jump should assemble correctly."
  (let ((code (format nil "CLC~% LDA #$00~% LDY #$00~% bne &loop~% nop~% nop~%
                           loop:~% INY~% bne &loop~% sbc $0001~% brk")))
    (cl-6502:execute cpu (asm code))
    (is (eql (cpu-ar cpu) 86))))

(deftest assemble-forward-refs
    "A program with forward references should assemble correctly."
  (let ((code (format nil "jmp $loop~% CLC~% LDA #$00~% LDY #$00~%
                           loop:~% INY~% bne &loop~% sbc $0001~% brk")))
    (is (equalp (asm code) #(76 8 0 24 169 0 160 0 200 208 253 237 1 0 0)))))

(deftest assemble-symbolic
    "A sexp-format program should assemble correctly."
  (is (equalp (asm '((:brk))) #(0)))
  (is (equalp (asm '((:nop))) #(234))))

(deftest assemble-symbolic-with-args
    "A sexp-format program with args should assemble correctly."
  (let ((code '((:ldy :#$00)
                (:iny)
                (:bne :&fd)
                (:sbc :$0001))))
    (is (equalp (asm code) #(160 0 200 208 253 237 1 0)))))

; (deftest assemble-pc "*" nil)?
