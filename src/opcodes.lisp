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
;; Current plan? (loop (let ((opcode (pc cpu)))
;;                       (setf (pc cpu) (wrap-word (1+ (pc cpu))))
;;                       (funcall (function (aref opcodes opcode)) opcode)))
;; This is wasteful of space and some computation but simple/entertaining.
;; Allows argument handling to reside with the methods not the main loop.
;; Allows the main loop to be pretty close to the above pseudocode.
;; Worth using sb-sprof sampling profiler to see just how much it hurts.

;;;; IDEAS:
;; 1) Store an array of methods, use ins as key, loop over instruction stream,
;; get and run method which retrieves the arguments from the stream or stack,
;; executes and then move on to the next thing.
;; 2) Have a giant case statement that knows what arguments are needed and calls
;; the appropriate opcode passing it the args.
;; 1) is the option I'm leaning towards but we don't really want to store the
;; method object returned by compute-applicable-methods. Instead, store the
;; symbols, retrieve and funcall. Seems like just about everybody does 2).
;; Also, I should read more Sonya Keene, clearly.

(defmacro defins ((name opcode cycle-count &key addr-mode)
                  (&key docs) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. When DOCS are provided
they serve as its docstring. Execute BODY in a let that binds CPU to *CPU* for
convenience. If ADDR-MODE is nil, implied (imp) addressing is assumed."
  ;; TODO: Add ecase for addr-mode options as needed.
  ;; TODO: Use symbol-plist for byte-count and disassembly format str/metadata?
  (declare (ignore addr-mode)) ; for now
  `(defmethod ,name ((opcode (eql ,opcode)))
     ,@(when docs (list docs))
     (let ((cpu *cpu*))
       ,@body)
     (incf (cc *cpu*) ,cycle-count)))

(defins (brk #x00 7)
    (:docs "Force Interrupt, 1 byte.")
  (let ((pc (wrap-word (1+ (pc cpu)))))
    (stack-push-word pc)
    (setf (status-bit 4) 1)
    (stack-push (sr cpu))
    (setf (status-bit 2) 1)
    (setf (pc cpu) (get-word #xfffe))))

(defins (ora #x01 6)
    (:docs "Bitwise OR Accumulator, 2 bytes.")
  (let ((result (setf (ar cpu) (logior (ar cpu) (indirect-x-address cpu)))))
    (if (zerop result)
        (setf (status-bit 1) 1)
        (setf (status-bit 7) 1))))
