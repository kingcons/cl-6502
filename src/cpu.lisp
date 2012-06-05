(in-package :6502-cpu)

;;;; REFERENCES:
;; http://en.wikipedia.org/wiki/Emulator#Structure_of_an_emulator
;; http://www.obelisk.demon.co.uk/6502/registers.html
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

(defun maybe-update-cycle-count (cpu address &optional start)
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count. If
START is provided, test that against ADDRESS. Otherwise, use (absolute cpu)."
  (when (not (= (logand (or start (absolute cpu)) #xff00)
                (logand address #xff00)))
    (incf (cpu-cc cpu))))

;;; Addressing
;; Notes:
; The common case is that we want the byte pointed to by an address,
; not the address itself. Unfortunately, stores require the actual address
; as we can't setf a value. Thus, we'll have these methods return addresses
; and defins will generate the get-byte calls. Implicit, Accumulator, and
; Indirect addressing modes can be implemented directly in the opcode and
; do not receive special support here.

(defmethod immediate ((cpu cpu))
  (cpu-pc cpu))

(defmethod zero-page ((cpu cpu))
  (get-byte (immediate cpu)))

(defmethod zero-page-x ((cpu cpu))
  (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))))

(defmethod zero-page-y ((cpu cpu))
  (wrap-byte (+ (zero-page cpu) (cpu-yr cpu))))

(defmethod indirect-x ((cpu cpu)) ;; aka Post-indexed Indirect
  (get-word (wrap-byte (+ (zero-page cpu) (cpu-xr cpu))) t))

(defmethod indirect-y ((cpu cpu)) ;; aka Pre-indexed Indirect
  (let* ((addr (get-word (zero-page-cpu) t))
         (result (wrap-word (+ addr (cpu-yr cpu)))))
    (maybe-update-cycle-count cpu result addr)
    result))

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
                  (&key docs) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. When DOCS are provided
they serve as its docstring. MODE must be a symbol or keyword. If MODE is a
symbol, it is funcalled to retrieve an address. If MODE is a keyword, it is
funcalled and get-byte is called on the subsequent address."
  ;; TODO: Use symbol-plist for byte-count and disassembly format str/metadata?
  (declare (ignore byte-count)) ; for now
  (let ((addr (etypecase mode
                (keyword `(lambda (cpu)
                            (get-byte (,(intern (princ-to-string mode)) cpu))))
                (symbol mode))))
    ;; KLUDGE: Why do I have to intern these symbols so they are created
    ;; in the correct package, i.e. the calling package rather than 6502-cpu?
    `(defmethod ,name ((,(intern "OPCODE") (eql ,opcode))
                       &key (,(intern "MODE") ,addr) (cpu *cpu*))
       ,@(when docs (list docs))
       ,@body
       (incf (cpu-cc cpu) ,cycle-count))))

(defmacro defopcode (name (&key docs) modes &body body)
  "Define instructions via DEFINS for each addressing mode listed in MODES
supplying DOCS and BODY appropriately."
  `(progn ,@(mapcar (lambda (mode)
                      `(defins (,name ,@mode)
                           (:docs ,docs)
                         ,@body))
                    modes)))
