(defpackage :6502
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-6502\">Github</a>")
  (:use :cl :alexandria)
  (:export #:*ram*
           #:*cpu*

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
           #:zero-page
           #:zero-page-x
           #:zero-page-y
           #:indirect-x
           #:indirect-y
           #:absolute
           #:absolute-x
           #:absolute-y
           #:branch-relative

           ;; Opcodes
           #:brk
           #:ora

           ;; Helpers
           #:reset))
