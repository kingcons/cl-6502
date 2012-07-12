(in-package :6502)

(defparameter *benchmark*
  '(;; Clear the Decimal and Carry status bits
    #xd8            ; $0000 d8        CLD
    #x18            ; $0001 18        CLC
    ;; Set the Accumulator and X and Y regs to zero
    #xa9 #x00       ; $0002 a9 00     LDA #$00
    #xa2 #x00       ; $0004 a2 00     LDX #$00
    #xa0 #x00       ; $0006 a0 00     LDY #$00
    ;; Increment Y until it wraps around to zero, then proceed to 0x0b
    #xc8            ; $0008 c8        INY
    #xd0 #xfd       ; $0009 d0 fd     BNE $0008
    ;; subtract the sum of value at 0x01+carry from accumulator and halt.
    ;; (remember that absolute addresses are stored low-byte first)
    #xed #x01 #x00  ; $000b ed 01 00  SBC #$0100
    #x00)           ; $000e 00        BRK
  "Should leave the Accumulator with the value 231.")
