(in-package :6502)

(defparameter *benchmark*
  (asm " ;; Clear the Carry status bit
         CLC
         ;; Set the Accumulator and X and Y regs to zero
         LDA #$00
         LDX #$00
         LDY #$00
         ;; Increment Y until it wraps around to zero, then proceed to 0x0b
         loop:
         INY
         BNE &loop
         ;; subtract the sum of value at 0x01+carry from accumulator and halt.
         ;; (remember that absolute addresses are stored low-byte first)
         SBC $0001
         BRK")
  "Should leave the Accumulator with the value 231.")

(defparameter *nop-loop*
  (asm "loop:
          nop
          jmp $loop")
  "Do nothing forever.")

; Thanks to Ed Spittles for his wisdom and assistance
(defun klaus-test ()
  "Run Klaus' functional testsuite."
  (6502::load-binary "projects/cl-6502/tests/6502_functional_test.bin")
  (setf (cpu-pc *cpu*) #x1000)
  (execute *cpu*))
