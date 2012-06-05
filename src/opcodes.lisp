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

(defopcode asl
    (:docs "Arithmetic Shift Left")
    ((#x06 5 2 :zero-page)
     (#x0a 2 1 :implied))
  ;; TODO: Implement carry handling.
  (let ((result (setf (cpu-ar cpu) (ash (cpu-ar cpu) 1))))
    (update-flags result)))

(defopcode brk
    (:docs "Force Break")
    ((#x00 7 1 :implied))
  (let ((pc (wrap-word (1+ (cpu-pc cpu)))))
    (stack-push-word pc)
    (setf (status-bit 4) 1)
    (stack-push (cpu-sr cpu))
    (setf (status-bit 2) 1)
    (setf (cpu-pc cpu) (get-word #xfffe))))

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
