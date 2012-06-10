(defpackage :6502-cpu
  (:use :cl :alexandria)
  (:export ;; Conditions
           #:6502-error
           #:not-implemented-yet
           #:illegal-opcode
           #:status-bit-error
           ;; CPU Struct
           #:cpu
           #:make-cpu
           #:copy-cpu
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
           #:next
           #:reset
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
           #:negative-p
           #:update-flags
           #:maybe-update-cycle-count
           #:branch-if
           ;; Opcode Macrology
           #:defopcode))

(defpackage :6502
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-6502\">Github</a>")
  (:use :cl :alexandria :6502-cpu)
  (:export ;; Opcodes
           #:adc
           #:and
           #:asl
           #:bcc
           #:bit
           #:bmi
           #:bpl
           #:brk
           #:bvc
           #:bvs
           #:clc
           #:cli
           #:dey
           #:eor
           #:jmp
           #:jsr
           #:lda
           #:ldx
           #:ldy
           #:lsr
           #:ora
           #:pha
           #:php
           #:pla
           #:plp
           #:rti
           #:rts
           #:sec
           #:sei
           #:sta
           #:stx
           #:sty
           #:txa
           #:txs
           #:tya)
  ;; Note: BE NOT AFRAID, DEVELOPERS! (But also, don't :use this package.)
  (:shadow #:bit
           #:and))
