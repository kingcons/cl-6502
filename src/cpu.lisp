(in-package :6502)

;;;; REFERENCES:
;; http://www.obelisk.demon.co.uk/6502/registers.html
;; http://www.obelisk.demon.co.uk/6502/addressing.html
;; http://nesdev.parodius.com/6502.txt

(deftype u8 () '(unsigned-byte 8))

(declaim (inline make-cpu))
(defstruct cpu
  "A 6502 CPU with an extra slot for tracking the cycle count/clock ticks."
  (pc #xfffc :type (unsigned-byte 16))  ;; program counter
  (sp #xfd   :type u8)                  ;; stack pointer
  (sr #x24   :type u8)                  ;; status register
  (xr 0      :type u8)                  ;; x register
  (yr 0      :type u8)                  ;; y register
  (ar 0      :type u8)                  ;; accumulator
  (cc 0      :type fixnum))             ;; cycle counter

(defmethod initialize-instance :after ((cpu cpu) &key)
  (setf (cpu-pc cpu) (absolute cpu)))

;;; Tasty Globals

(defparameter *ram* (make-array #x10000 :element-type 'u8)
  "A lovely hunk of bytes.")

(defparameter *cpu* (make-cpu)
  "The 6502 instance used by opcodes in the package.")

(defparameter *opcodes* (make-array #x100 :element-type 'cons
                                    :initial-element nil)
  "A mapping of opcodes to instruction mnemonic/metadata conses.")

;;; Helpers

(defgeneric reset (obj)
  (:documentation "Reset the OBJ to an initial state.")
  (:method (obj) (initialize-instance obj)))

(defgeneric nmi (obj)
  (:documentation "Generate a non-maskable interrupt. Used for vblanking in NES.")
  (:method (obj)
    (stack-push-word (cpu-pc obj) obj)
    (stack-push (cpu-sr obj) obj)
    (setf (cpu-pc obj) (get-word #xfffa))))

(defun get-instruction (opcode)
  "Get the mnemonic for OPCODE. Returns a symbol to be funcalled or nil."
  (first (aref *opcodes* opcode)))

(defun get-byte (address)
  "Get a byte from RAM at the given address."
  (aref *ram* address))

(defun (setf get-byte) (new-val address)
  "Set ADDRESS in *ram* to NEW-VAL."
  (setf (aref *ram* address) new-val))

(defun get-word (address &optional wrap-p)
  "Get a word from RAM starting at the given address."
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
  (let ((size (length bytevector)))
    (setf (subseq *ram* start (+ start size)) bytevector)))

(defun wrap-byte (val)
  "Wrap the given value to ensure it conforms to (typep val '(unsigned-byte 8)),
e.g. a Stack Pointer or general purpose register."
  (logand val #xff))

(defun wrap-word (val)
  "Wrap the given value to ensure it conforms to (typep val '(unsigned-byte 16)),
e.g. a Program Counter address."
  (logand val #xffff))

(defmethod wrap-stack ((cpu cpu))
  "Wrap the stack pointer."
  (setf (cpu-sp cpu) (wrap-byte (cpu-sp cpu))))

(defun wrap-page (address)
  "Wrap the given ADDRESS, ensuring that we don't cross a page boundary.
e.g. When the last two bytes of ADDRESS are #xff."
  (+ (logand address #xff00)
     (logand (1+ address) #xff)))

(defun stack-push (value cpu)
  "Push the given VALUE on the stack and decrement the SP."
  (setf (get-byte (+ (cpu-sp cpu) #x100)) (wrap-byte value))
  (decf (cpu-sp cpu))
  (wrap-stack cpu))

(defun stack-push-word (value cpu)
  "Push the 16-bit word VALUE onto the stack."
  (stack-push (wrap-byte (ash value -8)) cpu)
  (stack-push (wrap-byte value) cpu))

(defun stack-pop (cpu)
  "Pop the value pointed to by the SP and increment the SP."
  (incf (cpu-sp cpu))
  (wrap-stack cpu)
  (get-byte (+ (cpu-sp cpu) #x100)))

(defun stack-pop-word (cpu)
  "Pop a 16-bit word off the stack."
  (+ (stack-pop cpu) (ash (stack-pop cpu) 8)))

(defun %status-bit (key)
  (let ((status-register '((:carry     . 0)
                           (:zero      . 1)
                           (:interrupt . 2)
                           (:decimal   . 3)
                           (:break     . 4)
                           (:unused    . 5)
                           (:overflow  . 6)
                           (:negative  . 7))))
    (rest (assoc key status-register))))

(defun status-bit (key cpu)
  "Retrieve bit KEY from the status register of CPU. KEY should be a keyword."
  (ldb (byte 1 (%status-bit key)) (cpu-sr cpu)))

(defun (setf status-bit) (new-val key cpu)
  "Set bit KEY in the status reg of CPU to NEW-VAL. KEY should be a keyword."
  (setf (ldb (byte 1 (%status-bit key)) (cpu-sr cpu)) new-val))

(defmacro set-flags-if (cpu &rest flag-preds)
  "Takes any even number of arguments where the first is a keyword denoting a
status bit and the second is a funcallable predicate that takes no arguments.
It will set each flag to 1 if its predicate is true, otherwise 0."
  `(setf ,@(loop for (flag pred . nil) on flag-preds by #'cddr
              appending `((status-bit ,flag cpu) (if ,pred 1 0)))))

(defun set-flags-nz (cpu value)
  "Set the zero and negative bits of CPU's staus-register based on VALUE."
  (set-flags-if cpu :zero (zerop value) :negative (logbitp 7 value)))

(defun maybe-update-cycle-count (cpu address &optional start)
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count. If
START is provided, test that against ADDRESS. Otherwise, use (absolute cpu)."
  (when (not (= (logand (or start (absolute cpu)) #xff00)
                (logand address #xff00)))
    (incf (cpu-cc cpu))))

(defmacro branch-if (predicate cpu)
  "Take a Relative branch if PREDICATE is true, otherwise increment the PC."
  `(if ,predicate
       (setf (cpu-pc ,cpu) (relative ,cpu))
       (incf (cpu-pc ,cpu))))

(defun rotate-byte (integer count cpu)
  "Rotate the bits of INTEGER by COUNT. If COUNT is negative, rotate right."
  (let ((result (ash integer count)))
    (if (plusp (status-bit :carry cpu))
        (ecase count
          (01 (logior result #x01))
          (-1 (logior result #x80)))
        result)))

;;; Opcode Macrology

(defmacro defins ((name opcode cycle-count byte-count mode)
                  (&key setf-form (track-pc t)) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. MODE must return an
address or byte at an address if funcalled with a cpu. SETF-FORM is a lambda
that may be funcalled with a value to set the address computed by MODE. If
TRACK-PC is t, the default, the program counter will be incremented to just
past the instruction's operands. Otherwise, BODY is responsible for the PC."
  `(defmethod ,name ((,(intern "OPCODE") (eql ,opcode)) ,(intern "CPU") &key
                     (,(intern "MODE") ,mode) (,(intern "SETF-FORM") ,setf-form))
     ,@body
     ,@(when (cl:and track-pc (> byte-count 1))
         `((incf (cpu-pc cpu) ,(1- byte-count))))
     (incf (cpu-cc cpu) ,cycle-count)
     cpu))

(defmacro defopcode (name (&key (docs "") addr-style (track-pc t))
                     modes &body body)
  "Define a Generic Function NAME with DOCS if provided and instructions,
i.e. methods, via DEFINS for each addressing mode listed in MODES. If ADDR-STYLE
is :raw, MODE can be funcalled with a cpu in BODY to retrieve MODE's address. If
ADDR-STYLE is :mixed, accumulator addressing returns the accumulator value while
others return the byte at MODE's address. If ADDR-STYLE is nil, funcalling MODE
will return the byte at the given address."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       ,@(loop for (op cycles bytes mode) in modes
            collect `(setf (aref *opcodes* ,op) '(,name ,cycles ,bytes ,mode))))
     (defgeneric ,name (,(intern "OPCODE") ,(intern "CPU") &key
                         ,(intern "MODE") ,(intern "SETF-FORM"))
       (:documentation ,docs))
     ,@(loop for mode in modes for mname = (fourth mode) with x = (intern "X")
          do (setf (fourth mode)
                   (cond ((cl:and (eql addr-style :mixed)
                                  (eql mname 'accumulator)) `',mname)
                         ((eql addr-style :raw) `',mname)
                         (t `(lambda (cpu) (get-byte (,mname cpu))))))
          collect `(defins (,name ,@mode)
                       (:setf-form (lambda (,x) (setf (,mname cpu) ,x))
                        :track-pc ,track-pc)
                     ,@body))))
