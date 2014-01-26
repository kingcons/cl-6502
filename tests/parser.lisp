(in-package :6502-tests)

(def-suite parser :in 6502-tests)
(in-suite parser)

(deftest parse-just-opcode
  (is (equalp (6502::parse-code "brk") '(((:opcode . :brk))))))

(deftest parse-label
  (is (equalp (6502::parse-code "start:") '(((:label . "start"))))))

(deftest parse-opcode-with-operand
  (is (equalp (6502::parse-code "lda #123")
              '(((:value . 123) (:address-mode . :immediate)
                 (:opcode . :lda))))))

(deftest parse-operand-zeropage
  (is (equalp (6502::parse-code "lda $00")
              '(((:value . 0) (:opcode . :lda))))))

(deftest parse-operand-address
  (is (equalp (6502::parse-code "lda $2002")
              '(((:value . 8194) (:opcode . :lda))))))

(deftest parse-operand-accumulator
  (is (equalp (6502::parse-code "lsr a")
              '(((:value . "a") (:opcode . :lsr))))))

(deftest parse-operand-variable
  (is (equalp (6502::parse-code "lda some_label")
              '(((:value . "some_label") (:opcode . :lda))))))

(deftest parse-operand-indexing
  (is (equalp (6502::parse-code "lda $2002,x")
              '(((:value . 8194) (:opcode . :lda) (:indexing . :x))))))

(deftest parse-operand-add-addresses
  (is (equalp (6502::parse-code "lda 700+2")
              '(((:value . (+ 700 2)) (:opcode . :lda))))))

(deftest parse-operand-add-variable
  (is (equalp (6502::parse-code "lda some_label+2")
              '(((:value . (+ "some_label" 2)) (:opcode . :lda))))))

(deftest parse-operand-indexing
  (is (equalp (6502::parse-code "sta some_label,x")
              '(((:value . "some_label") (:opcode . :sta) (:indexing . :x))))))

(deftest parse-operand-indirect-ptr
  (is (equalp (6502::parse-code "sta (memory_ptr)")
              '(((:value . "memory_ptr") (:address-mode . :indirect)
                 (:opcode . :sta))))))

(deftest parse-operand-indirect-indexing
  (is (equalp (6502::parse-code "sta (memory_ptr),y")
              '(((:value . "memory_ptr") (:address-mode . :indirect)
                 (:opcode . :sta) (:indexing . :y))))))
