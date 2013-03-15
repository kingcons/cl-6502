(defpackage :6502
  (:use :cl)
  (:import-from :alexandria #:compose #:emptyp #:flatten #:make-keyword)
  (:shadow #:bit #:and)
  (:export ;; Public API
           #:execute #:6502-step #:asm #:disasm #:disasm-to-str #:disasm-to-list
           #:current-instruction #:get-byte #:get-word #:get-range #:*cpu* #:cpu
           #:nmi #:reset
           ;; CPU struct
           #:make-cpu #:cpu-ar #:cpu-xr #:cpu-yr #:cpu-sr #:cpu-sp #:cpu-pc #:cpu-cc #:u8 #:u16
           ;; Addr modes
           #:implied #:accumulator #:immediate #:zero-page #:zero-page-x #:zero-page-y
           #:absolute #:absolute-x #:absolute-y #:indirect #:indirect-x #:indirect-y #:relative
           ;; Helpers
           #:wrap-byte #:wrap-word #:status-bit #:get-instruction #:defenum #:bytevector))

(defpackage :cl-6502
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-6502\">Github</a>")
  (:use :cl)
  (:import-from :6502 #:execute #:6502-step #:asm #:disasm #:disasm-to-str #:disasm-to-list
                #:current-instruction #:get-byte #:get-word #:get-range #:*cpu* #:cpu #:nmi #:reset)
  (:export #:execute #:6502-step #:asm #:disasm #:disasm-to-str #:disasm-to-list
           #:current-instruction #:get-byte #:get-word #:get-range #:*cpu* #:cpu
           #:nmi #:reset))
