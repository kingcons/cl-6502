(in-package :6502-tests)

(def-suite jit :in 6502-tests)
(in-suite jit)

(deftest jit-passes-klaus-test
    "The JIT should pass Klaus Dorfmann's test suite."
  (klaus-init)
  (let ((cycles (* 78 (expt 2 20))))
    (loop until (> (cpu-cc *cpu*) cycles)
       do (6502::jit-step *cpu* (cpu-pc *cpu*))))
  (is (eql (cpu-pc *cpu*) #x3c37)))
