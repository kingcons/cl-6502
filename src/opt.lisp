(in-package :6502)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (set-dispatch-macro-character #\# #\f
     (lambda (stream sub-char numarg)
       (declare (ignore stream sub-char))
       (setq numarg (or numarg 3))
       (unless (<= numarg 3)
         (error "Bad value for #f: ~a" numarg))
       `(declare (optimize (speed ,numarg)
                           (safety ,(- 3 numarg)))))))
