(in-package :6502)

;;; http://www.obelisk.demon.co.uk/6502/addressing.html

(defgeneric reader (mode)
  (:documentation "Return a Perl-compatible regex suitable for parsing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defgeneric printer (mode)
  (:documentation "Return a format string suitable for printing MODE.")
  (:method (mode) (error 'invalid-mode :mode mode)))

(defmacro defaddress (name (&key cpu-reg reader printer (docs "")) &body body)
  "Define an Addressing Mode in the form of a method called NAME specialized on
CPU returning an address according to BODY and a setf function to store to that
address. If CPU-REG is non-nil, BODY will be wrapped in a get-byte for setf.
READER should be a Perl-compatible regex that can read assembly in the mode.
PRINTER should be the format string desired for disassembly of the mode. DOCS
is used as the documentation for the method and setf function when provided."
  `(progn
     (defgeneric ,name (cpu)
       (:documentation ,docs)
       (:method ((cpu cpu)) ,@body))
     (defmethod reader ((mode (eql ',name))) ,reader)
     (defmethod printer ((mode (eql ',name))) ,printer)
     (defun (setf ,name) (new-value cpu)
       ,docs
       ,(if cpu-reg
            `(setf ,@body new-value)
            `(let ((address (,name cpu)))
               (setf (get-byte address) new-value))))))

(defaddress implied (:reader "^$"
                     :printer "")
  nil)

(defaddress accumulator (:reader "^[aA]$"
                         :printer "A"
                         :cpu-reg t)
  (cpu-ar cpu))

(defaddress immediate (:reader "^#\\$[0-9a-fA-F]{2}$"
                       :printer "~{#$~2,'0x~}"
                       :cpu-reg t)
  (cpu-pc cpu))

(defaddress zero-page (:reader "^\\$[0-9a-fA-F]{2}$"
                       :printer "~{$~2,'0x~}")
  (get-byte (immediate cpu)))

(defaddress zero-page-x (:reader "^\\$[0-9a-fA-F]{2},[xX]$"
                         :printer "$~{~2,'0x~}, X")
  (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))))

(defaddress zero-page-y (:reader "^\\$[0-9a-fA-F]{2},[yY]$"
                         :printer "$~{~2,'0x~}, Y")
  (wrap-byte (+ (zero-page cpu) (cpu-yr cpu))))

(defaddress absolute (:reader "^\\$[0-9a-fA-F]{4}$"
                      :printer "$~{~2,'0x~}")
  (get-word (cpu-pc cpu)))

(defaddress absolute-x (:reader "^\\$[0-9a-fA-F]{4},[xX]$"
                        :printer "$~{~2,'0x~}, X")
  (let ((result (wrap-word (+ (absolute cpu) (cpu-xr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress absolute-y (:reader "^\\$[0-9a-fA-F]{4},[yY]$"
                        :printer "$~{~2,'0x~}, Y")
  (let ((result (wrap-word (+ (absolute cpu) (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress indirect (:reader "^\\(\\$[0-9a-fA-F]{4}\\)$"
                      :printer "($~{~2,'0x~})")
  (get-word (absolute cpu) t))

(defaddress indirect-x (:reader "^\\(\\$[0-9a-fA-F]{2}\\),[xX]$"
                        :printer "($~{~2,'0x~}), X")
  (get-word (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))) t))

(defaddress indirect-y (:reader "^\\(\\$[0-9a-fA-F]{2}\\),[yY]$"
                        :printer "($~{~2,'0x~}), Y")
  (let* ((addr (get-word (zero-page cpu) t))
         (result (wrap-word (+ addr (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result addr)
    result))

(defaddress relative (:reader "^&[0-9a-fA-F]{2}$"
                      :printer "&~{~2,'0x~}")
  (let ((addr (zero-page cpu))
        (result nil))
    (incf (cpu-cc cpu))
    (incf (cpu-pc cpu))
    (if (not (zerop (logand addr #x80)))
        (setf result (wrap-word (- (cpu-pc cpu) (logxor addr #xff) 1)))
        (setf result (wrap-word (+ (cpu-pc cpu) addr))))
    (maybe-update-cycle-count cpu result (cpu-pc cpu))
    result))
