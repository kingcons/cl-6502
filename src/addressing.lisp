;;; ### The Protocol

(in-package :6502)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mode-clauses* nil
    "A list of ecase clauses for each mode used by the GETTER/SETTER macros."))

(defun mode-body (mode)
  "Return the &BODY for a given addressing mode."
  (alexandria:if-let (body (rest (find mode *mode-clauses* :key #'first)))
    body
    (error 'invalid-mode :mode mode)))

(defgeneric reader (mode)
  (:documentation "Return a Perl-compatible regex suitable for parsing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defgeneric writer (mode)
  (:documentation "Return a format string suitable for printing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defmacro defaddress (name (&key reader writer) &body body)
  "Define an Addressing Mode that implements the protocol of GETTER,
SETTER, READER, and WRITER."
  `(progn
     (defmethod reader ((mode (eql ',name))) ,reader)
     (defmethod writer ((mode (eql ',name))) ,writer)
     (pushnew '(,name ,@body) *mode-clauses* :key #'first)))

(defun %getter (mode raw-p)
  "Get the value at MODE based on RAW-P."
  (let ((body (mode-body mode)))
    (if raw-p
        (first body)
        `(get-byte ,@body))))

(defun %getter-mixed (mode)
  "Special-case the handling of accumulator mode in ASL/LSR/ROL/ROR."
  (if (eql mode 'accumulator)
      (%getter mode t)
      (%getter mode nil)))

(defun %setter (mode value)
  "Set the memory at MODE to VALUE."
  (let ((body (mode-body mode)))
    (if (member mode '(immediate accumulator))
        `(setf ,@body ,value)
        `(setf (get-byte ,@body) ,value))))

;;; ### Addressing Modes

(defaddress implied (:reader "^$"
                     :writer "")
  nil)

(defaddress accumulator (:reader "^[aA]$"
                         :writer "A")
  (cpu-ar cpu))

(defaddress immediate (:reader "^#\\$[0-9a-fA-F]{2}$"
                       :writer "~{#$~2,'0x~}")
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
