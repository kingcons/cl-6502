(in-package :6502)

(defparameter *benchmark*
  '(#xd8       ; $0000 d8    CLD
    #x18       ; $0001 18    CLC
    #xa9 #x00  ; $0002 a9 00 LDA #$00
    #xa2 #x00  ; $0004 a2 00 LDX #$00
    #xa0 #x00  ; $0006 a0 00 LDY #$00
    #xc8       ; $0008 c8    INY
    #xd0 #xfd  ; $0009 d0 fd BNE $0008
    #xe8       ; $000b e8    INX
    #xd0 #xf8  ; $000c d0 f8 BNE $0006
    #x69 #x01  ; $000e 69 01 ADC #$01
    #xd0 #xf2  ; $0010 d0 f2 BNE $0004
    #x00       ; $0012 00    BRK
    #xed)      ; $0013 ed    SBC
  "Should leave the X register with the value 47.")

(defun my-bench (&optional (cpu *cpu*))
  (reset)
  (setf (get-range 0) *benchmark*)
  (execute cpu 0))

