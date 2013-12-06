(in-package :6502-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(def-suite performance :in 6502-tests)
(in-suite performance)

(defun profile! (&key (mode :cpu))
  "MODE may be :TIME, :CPU, or :ALLOC. Load Klaus' test suite and use SBCL's
statistical profiler to observe performance while running the test suite."
  (klaus-init)
  (sb-sprof:with-profiling (:sample-interval 0.001
                            :alloc-interval 2
                            :max-samples 1000
                            :show-progress t
                            :report :graph
                            :mode mode
                            :reset t)
    (loop until (> (cpu-cc *cpu*) (* 78 (expt 2 20)))
       do (step-cpu *cpu* (immediate *cpu*)))))

(deftest keep-it-fast
    "We should not have deteriorating performance. 5 seconds at most."
  ;; NOTE: This test based on 64-bit SBCL 1.1.4 on my Debian Thinkpad X200.
  (klaus-init)
  (let ((start (get-internal-real-time)))
    (klaus-test)
    (is (< (- (get-internal-real-time) start) #x1400))))
