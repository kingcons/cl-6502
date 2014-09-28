;;; ### Core Data Types

(in-package :6502)

(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))

(defstruct cpu
  "A 6502 CPU with an extra slot for tracking the cycle count/clock ticks."
  (pc #xfffc :type u16)                 ;; program counter
  (sp #xfd   :type u8)                  ;; stack pointer
  (sr #x24   :type u8)                  ;; status register
  (xr 0      :type u8)                  ;; x register
  (yr 0      :type u8)                  ;; y register
  (ar 0      :type u8)                  ;; accumulator
  (cc 0      :type fixnum))             ;; cycle counter

(defmethod initialize-instance :after ((cpu cpu) &key)
  (setf (cpu-pc cpu) (absolute cpu)))

(defun bytevector (size)
  "Return an array of the given SIZE with element-type u8."
  (make-array size :element-type 'u8))

;;; ### Tasty Globals

(declaim (type (simple-array u8 (#x10000)) *ram*))
(defparameter *ram* (bytevector #x10000)
  "A lovely hunk of bytes.")

(defparameter *cpu* (make-cpu)
  "The 6502 instance used by default during execution.")

(declaim (type (simple-vector 256) *opcode-funs*))
(defparameter *opcode-funs* (make-array #x100 :element-type '(or function null))
  "The opcode lambdas used during emulation.")

(defparameter *opcode-meta* (make-array #x100 :initial-element nil)
  "A mapping of opcodes to metadata lists.")

;;; ### Helpers

(defgeneric reset (obj)
  (:documentation "Reset the OBJ to an initial state.")
  (:method (obj) (initialize-instance obj)))

(defgeneric nmi (obj)
  (:documentation "Generate a non-maskable interrupt. Used for vblanking in NES.")
  (:method (obj)
    (stack-push-word (cpu-pc obj) obj)
    (stack-push (cpu-sr obj) obj)
    (setf (cpu-pc obj) (get-word #xfffa))))

(declaim (inline wrap-byte wrap-word wrap-page)
         (ftype (function (fixnum) u8) wrap-byte))
(defun wrap-byte (value)
  "Wrap VALUE so it conforms to (typep value 'u8), i.e. a single byte."
  (logand value #xff))

(declaim (ftype (function (fixnum) u16) wrap-word))
(defun wrap-word (value)
  "Wrap VALUE so it conforms to (typep value 'u16), i.e. a machine word."
  (logand value #xffff))

(defun wrap-page (address)
  "Wrap the given ADDRESS, ensuring that we don't cross a page boundary.
e.g. If we (get-word address)."
  (+ (logand address #xff00) (logand (1+ address) #xff)))

(declaim (ftype (function (u16) u8) get-byte))
(defun get-byte (address)
  "Get a byte from RAM at the given ADDRESS."
  (aref *ram* address))

(defun (setf get-byte) (new-val address)
  "Set ADDRESS in *ram* to NEW-VAL."
  (setf (aref *ram* address) new-val))

(defun get-word (address &optional wrap-p)
  "Get a word from RAM starting at the given ADDRESS."
  (+ (get-byte address)
     (ash (get-byte (if wrap-p (wrap-page address) (1+ address))) 8)))

(defun (setf get-word) (new-val address)
  "Set ADDRESS and (1+ ADDRESS) in *ram* to NEW-VAL, little endian ordering."
  (setf (get-byte address) (wrap-byte (ash new-val -8))
        (get-byte (1+ address)) (wrap-byte new-val)))

(defun get-range (start &optional end)
  "Get a range of bytes from RAM, starting from START and stopping at END if
provided."
  (subseq *ram* start end))

(defun (setf get-range) (bytevector start)
  "Replace the contents of RAM, starting from START with BYTEVECTOR."
  (setf (subseq *ram* start (+ start (length bytevector))) bytevector))

(declaim (inline stack-push stack-pop))
(defun stack-push (value cpu)
  "Push the byte VALUE on the stack and decrement the SP."
  (setf (get-byte (+ (cpu-sp cpu) #x100)) (wrap-byte value))
  (setf (cpu-sp cpu) (wrap-byte (1- (cpu-sp cpu)))))

(defun stack-push-word (value cpu)
  "Push the 16-bit word VALUE onto the stack."
  (stack-push (wrap-byte (ash value -8)) cpu)
  (stack-push (wrap-byte value) cpu))

(defun stack-pop (cpu)
  "Pop the value pointed to by the SP and increment the SP."
  (setf (cpu-sp cpu) (wrap-byte (1+ (cpu-sp cpu))))
  (get-byte (+ (cpu-sp cpu) #x100)))

(defun stack-pop-word (cpu)
  "Pop a 16-bit word off the stack."
  (+ (stack-pop cpu) (ash (stack-pop cpu) 8)))

(defmacro defenum (name (&rest keys))
  "Define a function named %NAME, that takes KEY as an arg and returns the
index of KEY. KEYS should be scalar values."
  (let ((enum (make-hash-table)))
    (loop for i = 0 then (1+ i)
       for key in keys
       do (setf (gethash key enum) i))
    `(defun ,(intern (format nil "%~:@(~A~)" name)) (key)
       (let ((enum ,enum))
         (gethash key enum)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defenum status-bit (:carry :zero :interrupt :decimal
                       :break :unused :overflow :negative)))

(defmacro status-bit (key)
  "Test if KEY is set in the status register. KEY should be a keyword."
  `(logand (cpu-sr cpu) ,(ash 1 (%status-bit key))))

(defmacro set-status-bit (key new-val)
  "Set bit KEY in the status reg to NEW-VAL. KEY should be a keyword."
  `(setf (ldb (byte 1 ,(%status-bit key)) (cpu-sr cpu)) ,new-val))

(defmacro set-flags-if (&rest flag-preds)
  "Takes any even number of arguments where the first is a keyword denoting a
status bit and the second is a funcallable predicate that takes no arguments.
It will set each flag to 1 if its predicate is true, otherwise 0."
  `(progn
     ,@(loop for (flag pred . nil) on flag-preds by #'cddr
          collecting `(set-status-bit ,flag (if ,pred 1 0)))))

(defun overflow-p (result reg mem)
  "Checks whether the sign of RESULT is found in the signs of REG or MEM."
  (flet ((sign-of (x) (logbitp 7 x)))
    (not (or (eql (sign-of result) (sign-of reg))
             (eql (sign-of result) (sign-of mem))))))

(defun maybe-update-cycle-count (cpu address &optional start)
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count. If
START is provided, test that against ADDRESS. Otherwise, use the absolute address."
  (let ((operand (or start (absolute cpu))))
    (declare (type u16 operand)
             (type u16 address)
             (type (or null u16) start))
    (when (not (= (logand operand #xff00)
                  (logand address #xff00)))
      (incf (cpu-cc cpu)))))

(defmacro branch-if (predicate)
  "Take a Relative branch if PREDICATE is true, otherwise increment the PC."
  `(if ,predicate
       (setf (cpu-pc cpu) (relative cpu))
       (incf (cpu-pc cpu))))

(defun rotate-byte (integer count cpu)
  "Rotate the bits of INTEGER by COUNT. If COUNT is negative, rotate right."
  (let ((result (ash integer count)))
    (if (plusp (status-bit :carry))
        (ecase count
          (01 (logior result #x01))
          (-1 (logior result #x80)))
        result)))

;;; ### Opcode Macrology

(defmacro defasm (name (&key (docs "") raw-p (track-pc t))
                  modes &body body)
  "Define a 6502 instruction NAME, storing its DOCS and metadata in *opcode-meta*,
and a lambda that executes BODY in *opcode-funs*. Within BODY, the functions
GETTER and SETTER can be used to get and set values for the current addressing
mode, respectively. TRACK-PC can be passed nil to disable program counter updates
for branching/jump operations. If RAW-P is true, GETTER will return the mode's
address directly, otherwise it will return the byte at that address. MODES is a
list of opcode metadata lists: (opcode cycles bytes mode)."
  `(progn
     ,@(loop for (op cycles bytes mode) in modes collect
            `(setf (aref *opcode-meta* ,op) ',(list name docs cycles bytes mode)))
     ,@(loop for (op cycles bytes mode) in modes collect
            `(setf (aref *opcode-funs* ,op)
                   (named-lambda ,(intern (format nil "~A-~X" name op)) (cpu)
                     (incf (cpu-pc cpu))
                     (flet ((getter ()
                              ,(make-getter name mode raw-p))
                            (setter (x)
                              (setf (,mode cpu) x)))
                       ,@body)
                     ,@(when track-pc
                         `((incf (cpu-pc cpu) ,(1- bytes))))
                     (incf (cpu-cc cpu) ,cycles))))))
