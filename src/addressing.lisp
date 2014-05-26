;;; ### The Protocol

(in-package :6502)

(defparameter *address-modes* nil
  "A list of all the 6502 Address Modes.")

(defgeneric reader (mode)
  (:documentation "Return a Perl-compatible regex suitable for parsing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defgeneric writer (mode)
  (:documentation "Return a format string suitable for printing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defmacro defaddress (name (&key reader writer cpu-reg) &body body)
  "Define an Addressing Mode, NAME, with READER and WRITER methods that take
   NAME as a symbol and return a regex or format expression, respectively,
   and a function and setf function to get and set the data pointed to by the
   given mode."
  `(progn
     (defmethod reader ((mode (eql ',name)))
       ,(cl-ppcre:regex-replace-all "_" reader "([^,()#&]+)"))
     (defmethod writer ((mode (eql ',name))) ,writer)
     (push ',name *address-modes*)
     (defun ,name (cpu) ,@body)
     (defun (setf ,name) (value cpu)
       ,(if cpu-reg
            `(setf ,@body value)
            `(setf (get-byte ,@body) value)))))

(defun make-getter (name mode raw-p)
  "Generate an appropriate GETTER for NAME based on RAW-P
and whether or not it is a register shift operation."
  (let ((register-shift-op-p (and (member name '(asl lsr rol ror))
                                     (eql mode 'accumulator))))
    (if (or raw-p register-shift-op-p)
        `(,mode cpu)
        `(get-byte (,mode cpu)))))

;;; ### Addressing Modes

(defaddress implied (:reader "^$"
                     :writer "")
  nil)

(defaddress accumulator (:reader "^[aA]$"
                         :writer "A"
                         :cpu-reg t)
  (cpu-ar cpu))

(defaddress immediate (:reader "^#_$"
                       :writer "~{#$~2,'0x~}"
                       :cpu-reg t)
  (cpu-pc cpu))

(defaddress zero-page (:reader "^_$"
                       :writer "~{$~2,'0x~}")
  (get-byte (cpu-pc cpu)))

(defaddress zero-page-x (:reader "^_,\\s*[xX]$"
                         :writer "$~{~2,'0x~}, X")
  (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-xr cpu))))

(defaddress zero-page-y (:reader "^_,\\s*[yY]$"
                         :writer "$~{~2,'0x~}, Y")
  (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-yr cpu))))

(defaddress absolute (:reader "^_$"
                      :writer "$~{~2,'0x~}")
  (get-word (cpu-pc cpu)))

(defaddress absolute-x (:reader "^_,\\s*[xX]$"
                        :writer "$~{~2,'0x~}, X")
  (let ((result (wrap-word (+ (get-word (cpu-pc cpu)) (cpu-xr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress absolute-y (:reader "^_,\\s*[yY]$"
                        :writer "$~{~2,'0x~}, Y")
  (let ((result (wrap-word (+ (get-word (cpu-pc cpu)) (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress indirect (:reader "^\\(_\\)$"
                      :writer "($~{~2,'0x~})")
  (get-word (get-word (cpu-pc cpu)) t))

(defaddress indirect-x (:reader "^\\(_\\),\\s*[xX]$"
                        :writer "($~{~2,'0x~}), X")
  (get-word (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-xr cpu))) t))

(defaddress indirect-y (:reader "^\\(_\\),\\s*[yY]$"
                        :writer "($~{~2,'0x~}), Y")
  (let* ((addr (get-word (get-byte (cpu-pc cpu)) t))
         (result (wrap-word (+ addr (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result addr)
    result))

(defaddress relative (:reader "^(&?_)$"
                      :writer "&~{~2,'0x~}")
  (let ((offset (get-byte (cpu-pc cpu))))
    (incf (cpu-cc cpu))
    (let ((result (if (logbitp 7 offset)
                      (wrap-word (- (cpu-pc cpu) (- #xff offset)))
                      (wrap-word (+ (cpu-pc cpu) (1+ offset))))))
      (maybe-update-cycle-count cpu result (1+ (cpu-pc cpu)))
      result)))
