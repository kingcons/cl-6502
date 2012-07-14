(in-package :6502)

(defparameter *benchmark*
  (asm " ;; Clear the Decimal and Carry status bits
         CLD
         CLC
         ;; Set the Accumulator and X and Y regs to zero
         LDA #$00
         LDX #$00
         LDY #$00
         ;; Increment Y until it wraps around to zero, then proceed to 0x0b
         INY
         BNE &fd
         ;; subtract the sum of value at 0x01+carry from accumulator and halt.
         ;; (remember that absolute addresses are stored low-byte first)
         SBC $0001
         BRK")
  "Should leave the Accumulator with the value 231.")
