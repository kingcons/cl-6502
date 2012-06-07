(in-package :6502)

;;;; REFERENCES:
;; http://www.obelisk.demon.co.uk/6502/reference.html
;; http://www.6502.org/tutorials/6502opcodes.html
;; http://nesdev.parodius.com/6502.txt
;; https://github.com/mnaberez/py65/blob/master/src/py65/devices/mpu6502.py

;;;; DESIGN:
;; Why methods and a case instead of functions inline in the case?
;; Well, now we can use :around methods and the MOP to be awesome/crazy.
;; Plus who wants one big case statement for a CPU? Ugh. Abstract!
;; Performance is less interesting than cool features and crazy hacks.
;; Optimize later! See Frodo redpill + ICU64 for an example of what's possible.
;; Worth using sb-sprof sampling profiler to find low hanging fruit.

(defopcode and
    (:docs "And with Accumulator")
    ((#x21 6 2 :indirect-x)
     (#x25 3 2 :zero-page)
     (#x29 2 2 :immediate)
     (#x2d 4 3 :absolute)
     (#x31 5 2 :indirect-y)
     (#x35 4 2 :zero-page-x)
     (#x39 4 3 :absolute-y)
     (#x3d 4 3 :absolute-x))
  (let ((result (setf (cpu-ar cpu) (logand (cpu-ar cpu) (funcall mode cpu)))))
    (update-flags result)))

;; TODO: Fix ASL.
;; Right now proper flags aren't set.
;; More importantly, sometimes we need to set a location in memory.
(defopcode asl
    (:docs "Arithmetic Shift Left" :raw t)
    ((#x06 5 2 :zero-page)
     (#x0a 2 1 :implied)
     (#x0e 6 3 :absolute)
     (#x16 6 2 :zero-page-x)
     (#x1e 7 3 :absolute-x))
  (let ((result nil))
    ;; KLUDGE: This breaks the "addressing mode oblivious" nature of the code.
    ;; It's unacceptable. Perhaps there should be dedicated addressing modes
    ;; for implied stuff? e.g. Accumulator
    (if (member 'implied mode)
        (setf result (setf (cpu-ar cpu) (ash (cpu-ar cpu) 1)))
        (setf result (setf (funcall mode cpu) (ash (cpu-ar cpu) 1))))
    (update-flags result)))

(defopcode bit
    (:docs "Test Bits in Memory with Accumulator")
    ((#x24 3 2 :zero-page)
     (#x2c 4 3 :absolute))
  (update-flags (funcall mode cpu) '(:zero :negative :overflow)))

(defopcode bpl
    (:docs "Branch on Positive Result")
    ((#x10 2 2 :relative))
  (branch-if (lambda () (zerop (status-bit :negative)))))

(defopcode brk
    (:docs "Force Break")
    ((#x00 7 1 :implied))
  (let ((pc (wrap-word (1+ (cpu-pc cpu)))))
    (stack-push-word pc)
    (setf (status-bit :break) 1)
    (stack-push (cpu-sr cpu))
    (setf (status-bit :interrupt) 1)
    (setf (cpu-pc cpu) (get-word #xfffe))))

(defopcode clc
    (:docs "Clear Carry Flag")
    ((#x18 2 1 :implied))
  (setf (status-bit :carry) 0))

(defopcode jsr
    (:docs "Jump, Saving Return Address" :raw t)
    ((#x20 6 3 :absolute))
  (stack-push-word (wrap-word (1+ (cpu-pc cpu))))
  (setf (cpu-pc cpu) (get-word (funcall mode cpu))))

(defopcode ora
    (:docs "Bitwise OR with Accumulator")
    ((#x01 6 2 :indirect-x)
     (#x05 3 2 :zero-page)
     (#x09 2 2 :immediate)
     (#x0d 4 3 :absolute)
     (#x10 4 3 :absolute-x)
     (#x11 5 2 :indirect-y)
     (#x15 4 2 :zero-page-x)
     (#x19 4 3 :absolute-y))
  (let ((result (setf (cpu-ar cpu) (logior (cpu-ar cpu) (funcall mode cpu)))))
    (update-flags result)))

(defopcode php
    (:docs "Push Processor Status")
    ((#x08 3 1 :implied))
  (stack-push (cpu-sr cpu)))

(defopcode plp
    (:docs "Pull Processor Status from Stack")
    ((#26 4 1 :implied))
  (setf (cpu-sr cpu) (stack-pop)))
