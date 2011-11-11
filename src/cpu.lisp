(in-package :6502)

;; REFERENCES:
;; http://en.wikipedia.org/wiki/Emulator#Structure_of_an_emulator
;; http://www.obelisk.demon.co.uk/6502/registers.html
;; http://code.google.com/p/applepy/source/browse/trunk/processor.py
;; https://github.com/jaoswald/cl-comfy-6502/blob/master/6502-opcodes.lisp

(defclass cpu ()
  ((pc :initarg :pc :initform #xfffc ;; program counter
       :accessor pc :type (unsigned-byte 16))
   (sp :initarg :sp :initform #xff ;; stack pointer
       :accessor sp :type (unsigned-byte 8))
   (sr :initarg :sr :initform 0 ;; status register
       :accessor sr :type (unsigned-byte 8))
   (xr :initarg :xr :initform 0 ;; x register
       :accessor xr :type (unsigned-byte 8))
   (yr :initarg :yr :initform 0 ;; y register
       :accessor yr :type (unsigned-byte 8))
   (ac :initarg :ac :initform 0 ;; accumulator
       :accessor ac :type (unsigned-byte 8))
   (cc :initarg :cc :initform 0 ;; cycle counter
       :accessor cc :type fixnum))
  (:documentation "A 6502 CPU with an additional non-register slot for tracking
the cycle count/clock ticks."))

(defmethod print-object ((object cpu) stream)
  "A method to print CPU instances as #<(6502) ...> where ... are the program
counter, stack pointer, x, y, and accumulator registers and their contents."
  (print-unreadable-object (object stream :type t)
    (with-slots (pc sp xr yr ac) object
      (format stream "(6502) PC=~x SP=~x X=~x Y=~x A=~x" pc sp xr yr ac))))

;; CPU helpers

(defun wrap-byte (val)
  "Wrap the given value to ensure it conforms to (typep val '(unsigned-byte 8))."
  (logand val 255))

(defmethod wrap-stack ((cpu cpu))
  "Wrap the stack pointer."
  (setf (sp cpu) (wrap-byte (sp cpu))))

(defmethod stack-push ((cpu cpu) value)
  "Push the given VALUE on the stack and decrement the SP."
  (setf (aref *ram* (+ (sp cpu) 256)) (wrap-byte value))
  (decf (sp cpu))
  (wrap-stack cpu))

(defmethod stack-pop ((cpu cpu))
  "Pop the value pointed to by the SP and increment the SP."
  (incf (sp cpu))
  (wrap-stack)
  (get-byte (+ (sp cpu) 256)))

(defmethod stack-push-word ((cpu cpu) value)
  "Push the 16-bit word VALUE onto the stack."
  (stack-push cpu (wrap-byte (ash value -8)))
  (stack-push cpu (wrap-byte value)))

(defmethod stack-pop-word ((cpu cpu))
  "Pop a 16-bit word off the stack."
  (+ (stack-pop cpu) (ash (stack-pop cpu) 8)))

(defun status-bit (n)
  "Retrieve bit N from the status register."
  (ldb (byte 1 n) (sr *cpu*)))

(defun (setf status-bit) (new-val n)
  "Set bit N in the status register to NEW-VAL."
  (setf (ldb (byte 1 n) (sr *cpu*)) new-val))

;;; Addressing Modes


;;; Tasty Globals

(defparameter *ram*
  (make-array 65536 :element-type '(unsigned-byte 8))
  "A lovely hunk of bytes.")

(defparameter *cpu* (make-instance 'cpu)
  "The 6502 instance used by opcodes in the package.")

;;; RAM helpers

(defun get-byte (address)
  "Get a byte from RAM at the given address."
  (aref *ram* address))

(defun get-word (address)
  "Get a word from RAM starting at the given address."
  (+ (get-byte address) (ash (get-byte (1+ address)) 8)))
