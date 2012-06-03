(in-package :6502-cpu)

;; REFERENCES:
;; http://en.wikipedia.org/wiki/Emulator#Structure_of_an_emulator
;; http://www.obelisk.demon.co.uk/6502/registers.html
;; http://code.google.com/p/applepy/source/browse/trunk/processor.py
;; https://github.com/jaoswald/cl-comfy-6502/blob/master/6502-opcodes.lisp

(defstruct cpu
  "A 6502 CPU with an extra slot for tracking the cycle count/clock ticks."
  (pc #xfffc :type (unsigned-byte 16)) ;; program counter
  (sp #xff   :type (unsigned-byte 8))  ;; stack pointer
  (sr #x30   :type (unsigned-byte 8))  ;; status register
  (xr 0      :type (unsigned-byte 8))  ;; x register
  (yr 0      :type (unsigned-byte 8))  ;; y register
  (ar 0      :type (unsigned-byte 8))  ;; accumulator
  (cc 0      :type fixnum))            ;; cycle counter

;;; Tasty Globals

(defparameter *ram* nil
  "A lovely hunk of bytes.")

(defparameter *cpu* nil
  "The 6502 instance used by opcodes in the package.")

;;; Helpers

(defun reset ()
  "Reset the virtual machine to an initial state."
  (setf *ram* (make-array (expt 2 16) :element-type '(unsigned-byte 8))
        *cpu* (make-cpu)))

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
i.e. a Stack Pointer or general purpose register."
  (logand val 255))

(defun wrap-word (val)
  "Wrap the given value to ensure it conforms to (typep val '(unsigned-byte 16)),
i.e. a Program Counter address."
  (logand val 65535))

(defmethod wrap-stack ((cpu cpu))
  "Wrap the stack pointer."
  (setf (cpu-sp cpu) (wrap-byte (cpu-sp cpu))))

(defun wrap-page (address)
  "Wrap the given ADDRESS, ensuring that we don't overflow. i.e. When the last
two bytes of ADDRESS are #xff."
  (+ (logand address #xff00)
     (logand (1+ address) #xff)))

(defun stack-push (value)
  "Push the given VALUE on the stack and decrement the SP."
  (setf (get-byte (+ (cpu-sp *cpu*) 256)) (wrap-byte value))
  (decf (cpu-sp *cpu*))
  (wrap-stack *cpu*))

(defun stack-push-word (value)
  "Push the 16-bit word VALUE onto the stack."
  (stack-push (wrap-byte (ash value -8)))
  (stack-push (wrap-byte value)))

(defun stack-pop ()
  "Pop the value pointed to by the SP and increment the SP."
  (incf (cpu-sp *cpu*))
  (wrap-stack *cpu*)
  (get-byte (+ (cpu-sp *cpu*) 256)))

(defun stack-pop-word ()
  "Pop a 16-bit word off the stack."
  (+ (stack-pop) (ash (stack-pop) 8)))

(defun status-bit (n)
  "Retrieve bit N from the status register."
  (ldb (byte 1 n) (cpu-sr *cpu*)))

(defun (setf status-bit) (new-val n)
  "Set bit N in the status register to NEW-VAL."
  (if (or (zerop new-val) (= 1 new-val))
      (setf (ldb (byte 1 n) (cpu-sr *cpu*)) new-val)
      (error 'status-bit-error :index n)))

(defun negative-p (value &optional (top-bit 7))
  "Returns T if the two's complement representation of a number is negative.
i.e. Has a 1 in the 7th bit position."
  (= 1 (ldb (byte 1 top-bit) value)))

(defun update-flags (value)
  "Set the zero and negative status bits based on VALUE."
  (setf (status-bit 1) (if (zerop value) 1 0)
        (status-bit 7) (if (negative-p value) 1 0)))

(defun maybe-update-cycle-count (cpu address)
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count."
  (when (not (= (logand (absolute cpu) #xff00)
                (logand address #xff00)))
    (incf (cpu-cc cpu))))

;;; Addressing
; Note: Implicit, Accumulator, and Indirect addressing modes can be
; implemented directly in the opcode and do not receive special support here.

(defmethod immediate ((cpu cpu))
  (zero-page cpu))

(defmethod zero-page ((cpu cpu))
  (get-byte (cpu-pc cpu)))

(defmethod zero-page-x ((cpu cpu))
  (wrap-byte (+ (cpu-xr cpu) (zero-page cpu))))

(defmethod zero-page-y ((cpu cpu))
  (wrap-byte (+ (cpu-yr cpu) (zero-page cpu))))

(defmethod indirect-x ((cpu cpu))
  (get-word (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))) t))

(defmethod indirect-y ((cpu cpu))
  (wrap-word (+ (get-word (zero-page cpu) t) (cpu-yr cpu))))

(defmethod absolute ((cpu cpu))
  (get-word (cpu-pc cpu)))

(defmethod absolute-x ((cpu cpu))
  (let ((result (wrap-word (+ (absolute cpu) (cpu-xr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defmethod absolute-y ((cpu cpu))
  (let ((result (wrap-word (+ (absolute cpu) (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defmethod branch-relative ((cpu cpu))
  (let ((addr (zero-page cpu)))
    (if (zerop (status-bit 1))
        (wrap-word (if (zerop (logand addr 128))
                       (- (cpu-pc cpu) (logxor addr 255))
                       (+ (cpu-pc cpu) addr)))
        (wrap-word (1+ (cpu-pc cpu))))))
