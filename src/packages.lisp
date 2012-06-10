(defpackage :6502-cpu
  (:use :cl :alexandria)
  (:export ;; Conditions
           #:not-implemented-yet
           #:illegal-opcode
           #:status-bit-error
           ;; Globals
           #:*cpu*
           ;; CPU Struct
           #:cpu
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
           #:rotate-byte
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
           #:bcs
           #:beq
           #:bit
           #:bmi
           #:bne
           #:bpl
           #:brk
           #:bvc
           #:bvs
           #:clc
           #:cld
           #:cli
           #:clv
           #:cmp
           #:cpx
           #:cpy
           #:dec
           #:dex
           #:dey
           #:eor
           #:inc
           #:inx
           #:iny
           #:jmp
           #:jsr
           #:lda
           #:ldx
           #:ldy
           #:lsr
           #:nop
           #:ora
           #:pha
           #:php
           #:pla
           #:plp
           #:rol
           #:ror
           #:rti
           #:rts
           #:sbc
           #:sec
           #:sed
           #:sei
           #:sta
           #:stx
           #:sty
           #:tax
           #:tay
           #:tsx
           #:txa
           #:txs
           #:tya)
  ;; Note: BE NOT AFRAID, DEVELOPERS! (But also, don't :use this package.)
  (:shadow #:bit
           #:and))
