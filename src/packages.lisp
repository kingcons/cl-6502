(defpackage :6502
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-6502\">Github</a>")
  (:use :cl :alexandria)
  (:export #:*ram*
           #:*cpu*
           #:cpu
           #:pc
           #:sp
           #:xr
           #:yr
           #:ar
           #:sr
           #:cc
           #:brk
           #:reset))
