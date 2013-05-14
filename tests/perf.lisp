(in-package :6502-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(def-suite performance :in 6502-tests)
(in-suite performance)

(defun profile-klaus-suite (&key (mode :time))
  "MODE may be :TIME, :CPU, or :ALLOC. Load Klaus' test suite and use SBCL's
statistical profiler to observe performance while running the test suite."
  (klaus-init)
  (sb-sprof:with-profiling (:max-samples 1000
                            :show-progress t
                            :report :graph
                            :mode mode
                            :reset t)
    (loop until (> (cpu-cc *cpu*) (* 45 (expt 2 21)))
       do (6502-step *cpu* (getter 'immediate t *cpu*)))))

(defun optimize-test ()
  (reset *cpu*)
  (klaus-init)
  (time (loop until (> (cpu-cc *cpu*) (* 1.79 (expt 2 20)))
           do (6502-step *cpu* (getter 'immediate t *cpu*)))))

(deftest keep-it-fast
    "We should not have deteriorating performance. 24.576 seconds at most."
  ;; NOTE: This test based on 64-bit SBCL 1.1.4 on my Debian Thinkpad X200.
  (klaus-init)
  (let ((start (get-internal-real-time)))
    (klaus-test)
    (is (< (- (get-internal-real-time) start) #x6000))))
