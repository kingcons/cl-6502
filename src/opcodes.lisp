(in-package :6502)

;; REFERENCES:
;; http://www.obelisk.demon.co.uk/6502/reference.html
;; http://www.obelisk.demon.co.uk/6502/addressing.html
;; http://www.6502.org/tutorials/6502opcodes.html
;; http://nesdev.parodius.com/6502.txt
;; https://github.com/mnaberez/py65/blob/master/src/py65/devices/mpu6502.py

;;;; DESIGN:
;; Why methods and a case instead of functions inline in the case?
;; Well, now we can use :around methods and the MOP to be awesome/crazy.
;; Plus who wants one big case statement for a CPU? Ugh. Abstract!
;; Performance is less interesting than cool features and crazy hacks.
;; Optimize later! See Frodo redpill + ICU64 for an example of what's possible.

;;;; NOTES:
;; I originally was thinking opcodes would specialize on the cpu instance and
;; dispatch addressing mode internally. Changed my mind. Eql specializers on
;; the opcodes themselves with instructions named after the mnemonics. The
;; remaining question is how to dispatch/call the method in the first place.

;;;; IDEAS:
;; 1) Store an array of methods, use ins as key, loop over instruction stream,
;; get and run method which retrieves the arguments from the stream or stack,
;; executes and then move on to the next thing.
;; 2) Have a giant case statement that knows what arguments are needed and calls
;; the appropriate opcode passing it the args.

(defmacro defins ((name opcode cycle-count)
                  (&key docs) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. When DOCS are provided
they serve as its docstring. Execute BODY in a let that binds CPU to *CPU* for
convenience."
  ;; TODO: Use symbol-plist for byte-count and disassembly format str/metadata?
  ;; Maybe have it vector-push-extend onto a *opcodes* array?
  `(defmethod ,name ((opcode (eql ,opcode)))
     ,@(when docs (list docs))
     (let ((cpu *cpu*))
       ,@body)
     (incf (cc *cpu*) ,cycle-count)))

(defins (brk #x00 7) ; Implied addressing, 1 byte
  (:docs "Force Interrupt.")
  (let ((pc (logand (1+ (pc cpu)) #xffff)))
    (stack-push-word cpu pc)
    (setf (status-bit 4) 1) ; set break flag
    (stack-push cpu (sr cpu))
    (setf (status-bit 2) 1) ; set interrupt flag
    (setf (pc cpu) (get-word #xfffe))))
