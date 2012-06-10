(in-package :6502-tests)

(def-fixture cpu ()
  (let ((*ram* (make-array (expt 2 16) :element-type '(unsigned-byte 8)))
        (*cpu* (6502-cpu::make-cpu)))
    (declare (ignore *ram*))
    (&body)))

(defmacro deftest (name docstring &body body)
  `(test ,name
     ,docstring
     (with-fixture cpu ()
       ,@body)))
