(in-package :6502-tests)

(def-suite :addressing)
(in-suite :addressing)

(test immediate)

(test zero-page)

(test zero-page-x)

(test zero-page-y)

(test indirect-x)

(test indirect-y)

(test absolute)

(test absolute-x)

(test absolute-y)

(test branch-relative)

;; are immediate and zero-page addressing /really/ the same?
;; what differences need i account for?
;; they are close. immediate fetches next addr. zpg gets it 00xx for xx=getPCi
