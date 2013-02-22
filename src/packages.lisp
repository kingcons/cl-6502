(defpackage :6502-cpu
  (:use :cl :alexandria)
  (:export ;; Conditions
           #:illegal-opcode
           #:invalid-mode
           #:invalid-syntax
           ;; Types
           #:u8
           ;; Globals
           #:*cpu*
           ;; CPU Struct
           #:cpu
           #:make-cpu
           #:cpu-pc
           #:cpu-sp
           #:cpu-xr
           #:cpu-yr
           #:cpu-ar
           #:cpu-sr
           #:cpu-cc
           ;; Addressing Modes
           #:implied
           #:accumulator
           #:immediate
           #:zero-page
           #:zero-page-x
           #:zero-page-y
           #:indirect
           #:indirect-x
           #:indirect-y
           #:absolute
           #:absolute-x
           #:absolute-y
           #:relative
           ;; Utils
           #:asm
           #:disasm
           #:disasm-to-str
           #:reset
           #:nmi
           #:get-instruction
           #:get-byte
           #:get-word
           #:get-range
           #:wrap-byte
           #:wrap-word
           #:wrap-stack
           #:wrap-page
           #:stack-push
           #:stack-push-word
           #:stack-pop
           #:stack-pop-word
           #:status-bit
           #:set-flags-if
           #:set-flags-nz
           #:maybe-update-cycle-count
           #:branch-if
           #:rotate-byte
           ;; Opcode Macrology
           #:defopcode
           #:defins))

(defpackage :6502
  (:use :cl :alexandria :6502-cpu)
  (:export ;; Utils/UI
           #:execute
           #:6502-step)
  ;; Note: BE NOT AFRAID, DEVELOPERS! (But also, don't :use this package.)
  (:shadow #:bit
           #:and))

(defpackage :cl-6502
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-6502\">Github</a>")
  (:use :cl)
  (:import-from :6502 #:*cpu*
                      #:reset
                      #:6502-step
                      #:execute
                      #:asm
                      #:disasm
                      #:disasm-to-str
                      #:get-byte
                      #:get-word
                      #:get-range)
  (:export #:*cpu*
           #:reset
           #:6502-step
           #:execute
           #:asm
           #:disasm
           #:disasm-to-str
           #:get-byte
           #:get-word
           #:get-range))
