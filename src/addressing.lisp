;;; ### The Protocol

(in-package :6502)

(defgeneric reader (mode)
  (:documentation "Return a Perl-compatible regex suitable for parsing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defgeneric writer (mode)
  (:documentation "Return a format string suitable for printing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defmacro defaddress (name (&key reader writer cpu-reg) &body body)
  ;; TODO: Update docstring, book. Test!
  "Define an Addressing Mode that implements the protocol of GETTER,
SETTER, READER, and WRITER."
  `(progn
     (defmethod reader ((mode (eql ',name))) ,reader)
     (defmethod writer ((mode (eql ',name))) ,writer)
     (defun ,name (cpu) ,@body)
     (defun (setf ,name) (value cpu)
       ,(if cpu-reg
            `(setf ,@body value)
            `(setf (get-byte ,@body) value)))))

;;; ### Addressing Modes

(defaddress implied (:reader "^$"
                     :writer "")
  nil)

(defaddress accumulator (:reader "^[aA]$"
                         :writer "A"
                         :cpu-reg t)
  (cpu-ar cpu))

(defaddress immediate (:reader "^#\\$[0-9a-fA-F]{2}$"
                       :writer "~{#$~2,'0x~}"
                       :cpu-reg t)
  (cpu-pc cpu))

(defaddress zero-page (:reader "^\\$[0-9a-fA-F]{2}$"
                       :writer "~{$~2,'0x~}")
  (get-byte (cpu-pc cpu)))

(defaddress zero-page-x (:reader "^\\$[0-9a-fA-F]{2},[xX]$"
                         :writer "$~{~2,'0x~}, X")
  (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-xr cpu))))

(defaddress zero-page-y (:reader "^\\$[0-9a-fA-F]{2},[yY]$"
                         :writer "$~{~2,'0x~}, Y")
  (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-yr cpu))))

(defaddress absolute (:reader "^\\$[0-9a-fA-F]{4}$"
                      :writer "$~{~2,'0x~}")
  (get-word (cpu-pc cpu)))

(defaddress absolute-x (:reader "^\\$[0-9a-fA-F]{4},[xX]$"
                        :writer "$~{~2,'0x~}, X")
  (let ((result (wrap-word (+ (get-word (cpu-pc cpu)) (cpu-xr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress absolute-y (:reader "^\\$[0-9a-fA-F]{4},[yY]$"
                        :writer "$~{~2,'0x~}, Y")
  (let ((result (wrap-word (+ (get-word (cpu-pc cpu)) (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress indirect (:reader "^\\(\\$[0-9a-fA-F]{4}\\)$"
                      :writer "($~{~2,'0x~})")
  (get-word (get-word (cpu-pc cpu)) t))

(defaddress indirect-x (:reader "^\\(\\$[0-9a-fA-F]{2}\\),[xX]$"
                        :writer "($~{~2,'0x~}), X")
  (get-word (wrap-byte (+ (get-byte (cpu-pc cpu)) (cpu-xr cpu))) t))

(defaddress indirect-y (:reader "^\\(\\$[0-9a-fA-F]{2}\\),[yY]$"
                        :writer "($~{~2,'0x~}), Y")
  (let* ((addr (get-word (get-byte (cpu-pc cpu)) t))
         (result (wrap-word (+ addr (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result addr)
    result))

(defaddress relative (:reader "^&[0-9a-fA-F]{2}$"
                      :writer "&~{~2,'0x~}")
  (let ((offset (get-byte (cpu-pc cpu))))
    (incf (cpu-cc cpu))
    (let ((result (if (logbitp 7 offset)
                      (wrap-word (- (cpu-pc cpu) (- #xff offset)))
                      (wrap-word (+ (cpu-pc cpu) (1+ offset))))))
      (maybe-update-cycle-count cpu result (1+ (cpu-pc cpu)))
      result)))
