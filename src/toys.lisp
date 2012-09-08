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
         loop:
         INY
         BNE &loop
         ;; subtract the sum of value at 0x01+carry from accumulator and halt.
         ;; (remember that absolute addresses are stored low-byte first)
         SBC $0001
         BRK")
  "Should leave the Accumulator with the value 231.")

; After (setf (get-range 0) *nop-loop*) do...
; (time (loop until (> (cpu-cc *cpu*) 1800000) do (6502-step *cpu* (zero-page *cpu*))))
; Maybe (require 'sb-sprof) (with-profiling (:max-samples 1000 :report :flat :show-progress t) ...)
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
