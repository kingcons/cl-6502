(in-package :6502)

(defparameter *benchmark*
  '(;; Clear the Decimal and Carry status bits
    #xd8       ; $0000 d8    CLD
    #x18       ; $0001 18    CLC
    ;; Set the Accumulator and X and Y regs to zero
    #xa9 #x00  ; $0002 a9 00 LDA #$00
    #xa2 #x00  ; $0004 a2 00 LDX #$00
    #xa0 #x00  ; $0006 a0 00 LDY #$00
    ;; Increment Y, then branch to 0x08
    #xc8       ; $0008 c8    INY
    #xd0 #xfd  ; $0009 d0 fd BNE $0008
    ;; Increment X until zero bit set, branch to 0x06, delete y and loop
    #xe8       ; $000b e8    INX
    #xd0 #xf8  ; $000c d0 f8 BNE $0006
    ;; Add 1 with carry to accumulator and rewind to 0x04
    #x69 #x01  ; $000e 69 01 ADC #$01
    #xd0 #xf2  ; $0010 d0 f2 BNE $0004
    ;; set break and interrupt, set PC to 0, subtract from accumulator.
    #x00       ; $0012 00    BRK
    #xed)      ; $0013 ed    SBC
  "Should leave the Y register with the value 47.")

(defun my-bench (&optional (cpu *cpu*))
  (reset)
  (setf (get-range 0) *benchmark*
        (cpu-pc cpu) 0)
  (execute cpu))
