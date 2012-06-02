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

(defvar *opcodes* (make-array 256 :element-type 'symbol))

(defmacro defins ((name opcode cycle-count byte-count &optional mode)
                  (&key docs) &body body)
  "Define an EQL-Specialized method on OPCODE named NAME. When DOCS are provided
they serve as its docstring."
  ;; TODO: Use symbol-plist for byte-count and disassembly format str/metadata?
  (declare (ignore byte-count)) ; for now
  `(progn
     (defmethod ,name ((opcode (eql ,opcode))
                       &key (mode ,mode) (cpu *cpu*))
       ,@(when docs (list docs))
       ,@body
       (incf (cpu-cc *cpu*) ,cycle-count))
     (setf (aref *opcodes* ,opcode) ',name)))

(defmacro defopcode (name (&key docs) modes &body body)
  "Define instructions via DEFINS for each addressing mode listed in MODES
supplying DOCS and BODY appropriately."
  `(progn ,@(mapcar (lambda (mode)
                      `(defins (,name ,@mode)
                           (:docs ,docs)
                         ,@body))
                    modes)))

(defopcode asl
    (:docs "Arithmetic Shift Left")
    ((#x06 5 2 'zero-page)
     (#x0a 2 1 'implied))
  (let ((result (setf (cpu-ar cpu) (ash (cpu-ar cpu) 1))))
    (update-flags result)))

(defopcode brk
    (:docs "Force Interrupt.")
    ((#x00 7 1 'implied))
  (let ((pc (wrap-word (1+ (cpu-pc cpu)))))
    (stack-push-word pc)
    (setf (status-bit 4) 1)
    (stack-push (cpu-sr cpu))
    (setf (status-bit 2) 1)
    (setf (cpu-pc cpu) (get-word #xfffe))))

(defopcode ora
    (:docs "Bitwise OR Accumulator")
  ((#x01 6 2 'indirect-x)
   (#x05 3 2 'zero-page)
   (#x09 2 2 'immediate)
   (#x0d 4 3 'absolute))
  (let ((result (setf (cpu-ar cpu) (logior (cpu-ar cpu) (funcall mode cpu)))))
    (update-flags result)))

(defopcode php
    (:docs "Push Processor Status")
    ((#x08 3 1 'implied))
  (stack-push (cpu-sr cpu)))
