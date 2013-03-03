(defpackage :6502-tests
  (:use :cl :fiveam :6502)
  (:import-from :6502-conf #:app-path)
  (:import-from :alexandria #:read-file-into-byte-vector)
  (:export #:run!))
