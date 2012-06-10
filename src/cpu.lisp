(in-package :6502-cpu)

;;;; REFERENCES:
;; http://www.obelisk.demon.co.uk/6502/registers.html
;; http://www.obelisk.demon.co.uk/6502/addressing.html
;; http://code.google.com/p/applepy/source/browse/trunk/processor.py
;; https://github.com/jaoswald/cl-comfy-6502/blob/master/6502-opcodes.lisp

(defstruct cpu
  "A 6502 CPU with an extra slot for tracking the cycle count/clock ticks."
  (pc #xfffc :type (unsigned-byte 16))  ;; program counter
  (sp #xff   :type (unsigned-byte 8))   ;; stack pointer
  (sr #x30   :type (unsigned-byte 8))   ;; status register
  (xr 0      :type (unsigned-byte 8))   ;; x register
  (yr 0      :type (unsigned-byte 8))   ;; y register
  (ar 0      :type (unsigned-byte 8))   ;; accumulator
  (cc 0      :type fixnum))             ;; cycle counter

;;; Tasty Globals

(defparameter *ram* (make-array (expt 2 16) :element-type '(unsigned-byte 8))
  "A lovely hunk of bytes.")

(defparameter *cpu* (make-cpu)
  "The 6502 instance used by opcodes in the package.")

(defparameter *opcodes*
  #(brk ora nil nil nil ora asl nil php ora asl nil nil ora asl
    bpl ora nil nil nil ora asl nil clc ora nil nil nil ora asl
    jsr and nil nil bit and rol nil plp and rol nil bit and rol
    bmi and nil nil nil and rol nil sec and nil nil nil and rol
    rti eor nil nil nil eor lsr nil pha eor lsr nil jmp eor lsr
    bvc eor nil nil nil eor lsr nil cli eor nil nil nil eor lsr
    rts adc nil nil nil adc ror nil pla adc ror nil jmp adc ror
    bvs adc nil nil nil adc ror nil sei adc nil nil nil adc ror
    nil sta nil nil sty sta stx nil dey nil txa nil sty sta stx
    bcc sta nil nil sty sta stx nil tya sta txs nil nil sta nil
    ldy lda ldx nil ldy lda ldx nil tay lda tax nil ldy lda ldx
    bcs lda nil nil ldy lda ldx nil clv lda tsx nil ldy lda ldx
    cpy cmp nil nil cpy cmp dec nil iny cmp dex nil cpy cmp dec
    bne cmp nil nil nil cmp dec nil cld cmp nil nil nil cmp dec
    cpx sbc nil nil cpx sbc inc nil inx sbc nop nil cpx sbc inc
    beq sbc nil nil nil sbc inc nil sed sbc nil nil nil sbc inc)
  "A mapping of opcodes to instruction mnemonics.")

;;; Helpers

(defmethod next ((cpu cpu)) ;; Rename and shadow STEP?
  "Step the CPU through the next instruction."
  (let ((opcode (immediate cpu)))
    (setf (cpu-pc cpu) (wrap-word (1+ (cpu-pc cpu))))
    (handler-case (funcall (get-instruction opcode) opcode)
      (undefined-function ()
        (error 'illegal-opcode :opcode opcode))
      ;; KLUDGE: Catch simple-error for now as there isn't a
      ;; no-applicable-method condition. A suggestion from pjb:
      ;; (defmethod no-applicable-method ((fun t) &rest args) (error ...))
      (simple-error ()
        (error 'not-yet-implemented :opcode opcode)))))

(defun reset ()
  "Reset the virtual machine to an initial state."
  (setf *ram* (make-array (expt 2 16) :element-type '(unsigned-byte 8))
        *cpu* (make-cpu)))

(defun get-instruction (opcode)
  "Get the mnemonic for OPCODE. Returns a symbol to be funcalled."
  (aref *opcodes* opcode))

(defun (setf get-instruction) (sym opcode)
  "Set the mnemonic for OPCODE to SYM. SYM should be a funcallable symbol."
  (setf (aref *opcodes* opcode) sym))

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

(defun stack-push (value)
  "Push the given VALUE on the stack and decrement the SP."
  (setf (get-byte (+ (cpu-sp *cpu*) #xff)) (wrap-byte value))
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
  (get-byte (+ (cpu-sp *cpu*) #xff)))

(defun stack-pop-word ()
  "Pop a 16-bit word off the stack."
  (+ (stack-pop) (ash (stack-pop) 8)))

(defun %status-bit (n)
  (let ((status-register '((:carry     . 0)
                           (:zero      . 1)
                           (:interrupt . 2)
                           (:decimal   . 3)
                           (:break     . 4)
                           (:unused    . 5)
                           (:overflow  . 6)
                           (:negative  . 7))))
    (rest (assoc n status-register))))

(defun status-bit (n)
  "Retrieve bit N from the status register. N should be a keyword."
  (ldb (byte 1 (%status-bit n)) (cpu-sr *cpu*)))

(defun (setf status-bit) (new-val n)
  "Set bit N in the status register to NEW-VAL. N should be a keyword."
  (if (or (zerop new-val) (= 1 new-val))
      (setf (ldb (byte 1 (%status-bit n)) (cpu-sr *cpu*)) new-val)
      (error 'status-bit-error :index (%status-bit n))))

(defun update-flags (value &optional (flags '(:zero :negative)))
  "Loop over FLAGS which should be a list of keywords and set flags in the
status register based on VALUE. FLAGS is '(:zero :negative) by default."
  (loop for flag in flags do
    (setf (status-bit flag)
          (ecase flag ; TODO: Do carry and negative always work as expected?
            (:zero (if (zerop value) 1 0))
            (:carry (if (logbitp 7 value) 1 0))
            (:negative (if (logbitp 7 value) 1 0))
            (:overflow (if (logbitp 6 value) 1 0))))))

(defun maybe-update-cycle-count (cpu address &optional start)
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count. If
START is provided, test that against ADDRESS. Otherwise, use (absolute cpu)."
  (when (not (= (logand (or start (absolute cpu)) #xff00)
                (logand address #xff00)))
    (incf (cpu-cc cpu))))

(defun branch-if (predicate)
  "Take a Relative branch if PREDICATE is true, otherwise increment the PC."
  (if (funcall predicate)
      (setf (cpu-pc *cpu*) (relative *cpu*))
      (incf (cpu-pc *cpu*))))

; Stolen and slightly hacked up from Cliki. Thanks cliki!
(defun rotate-byte (integer &optional (count 1) (size 8))
  "Rotate the bits of INTEGER by COUNT. If COUNT is negative, rotate right
instead of left. SIZE specifies the bitlength of the integer being rotated."
  (let* ((count (mod count size))
         (bytespec (byte size 0)))
    (labels ((rotate-byte-from-0 (count integer)
               (if (> count 0)
                   (logior (ldb bytespec (ash integer count))
                           (ldb bytespec (ash integer (- count size))))
                   (logior (ldb bytespec (ash integer count))
                           (ldb bytespec (ash integer (+ count size)))))))
      (dpb (rotate-byte-from-0 count (ldb bytespec integer))
           bytespec
           integer))))

;;; Addressing

; Usually we want the byte pointed to by an address, not the address.
(defmacro defaddress (name (&key cpu-reg) &body body)
  "Define an Addressing Mode in the form of a method called NAME specialized on
CPU returning an address according to BODY and a setf function to store to that
address. If CPU-REG is non-nil, BODY will be wrapped in a get-byte for setf."
  `(progn
     (defmethod ,name ((cpu cpu))
       ,@body)
     (defun (setf ,name) (new-value cpu)
       ,(if cpu-reg
            `(setf ,@body new-value)
            `(let ((address (,name cpu)))
               (setf (get-byte address) new-value))))))

(defmethod implied ((cpu cpu))
  nil)

(defaddress accumulator (:cpu-reg t)
  (cpu-ar cpu))

(defaddress immediate (:cpu-reg t)
  (cpu-pc cpu))

(defaddress zero-page ()
  (get-byte (immediate cpu)))

(defaddress zero-page-x ()
  (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))))

(defaddress zero-page-y ()
  (wrap-byte (+ (zero-page cpu) (cpu-yr cpu))))

(defaddress absolute ()
  (get-word (cpu-pc cpu)))

(defaddress absolute-x ()
  (let ((result (wrap-word (+ (absolute cpu) (cpu-xr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress absolute-y ()
  (let ((result (wrap-word (+ (absolute cpu) (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result)
    result))

(defaddress indirect ()
  (get-word (absolute cpu)))

(defaddress indirect-x ()
  (get-word (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))) t))

(defaddress indirect-y ()
  (let* ((addr (get-word (zero-page cpu) t))
         (result (wrap-word (+ addr (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result addr)
    result))

(defaddress relative ()
  (let ((addr (zero-page cpu))
        (result nil))
    (incf (cpu-cc cpu))
    (incf (cpu-pc cpu))
    (if (not (zerop (logand addr #x80)))
        (setf result (wrap-word (- (cpu-pc cpu) (logxor addr #xff) 1)))
        (setf result (wrap-word (+ (cpu-pc cpu) addr))))
    (maybe-update-cycle-count cpu result (cpu-pc cpu))
    result))

;;; Opcode Macrology

(defmacro defins ((name opcode cycle-count byte-count mode)
                  (&key setf-form raw) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. MODE can be funcalled to
retrieve the byte at the specified address mode. SETF-FORM is a lambda that may be
funcalled with a value to set the address computed by MODE. If RAW is nil, mode will
be a lambda that takes a cpu and returns the byte at the address mode specified."
  ;; TODO: Use symbol-plist for byte-count and disassembly format str/metadata?
  (declare (ignore byte-count)) ; for now
  (let ((mode `(lambda (cpu) (get-byte (,(second mode) cpu)))))
    ;; KLUDGE: Why do I have to intern these symbols so they are created
    ;; in the correct package, i.e. the calling package rather than 6502-cpu?
    `(defmethod ,name ((,(intern "OPCODE") (eql ,opcode)) &key (cpu *cpu*)
                       (,(intern "MODE") ,mode) (,(intern "SETF-FORM") ,setf-form))
       ,@body
       (incf (cpu-cc cpu) ,cycle-count))))

(defmacro defopcode (name (&key docs raw) modes &body body)
  "Define a Generic Function NAME with DOCS if provided and instructions,
i.e. methods, via DEFINS for each addressing mode listed in MODES. If RAW is
non-nil, MODE will be a symbol to funcall to get an address rather than a byte."
  `(progn
     (defgeneric ,name (opcode &key cpu mode setf-form)
       (:documentation ,docs))
     ,@(mapcar (lambda (mode)
                 (let ((mode-name (second (alexandria:lastcar mode))))
                   `(defins (,name ,@mode)
                        (:raw ,raw
                         :setf-form (lambda (x) (setf (,mode-name cpu) x)))
                      ,@body)))
               modes)))
