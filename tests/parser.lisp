(in-package :6502-tests)

(def-suite parser :in 6502-tests)
(in-suite parser)

(deftest parse-just-opcode
  (let ((expect (6502::make-instruction
                 :opcode :brk
                 :address-mode '(6502::implied))))
    (is (equalp (list expect) (6502::parse-code "brk")))))

(deftest parse-label
  (let ((expect (6502::make-instruction
                 :label "start"
                 :address-mode '(6502::implied))))
    (is (equalp (list expect) (6502::parse-code "start:")))))

(deftest parse-opcode-with-operand
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value 123
                 :address-mode '(6502::immediate))))
    (is (equalp (list expect) (6502::parse-code "lda #123")))))

(deftest parse-operand-zeropage
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value 0
                 :address-mode '(6502::relative 6502::absolute 6502::zero-page))))
    (is (equalp (list expect) (6502::parse-code "lda $00")))))

(deftest parse-operand-address
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value 8194
                 :address-mode '(6502::relative 6502::absolute 6502::zero-page))))
    (is (equalp (list expect) (6502::parse-code "lda $2002")))))

(deftest parse-operand-accumulator
  (let ((expect (6502::make-instruction
                 :opcode :lsr
                 :value "a"
                 :address-mode '(6502::accumulator))))
    (is (equalp (list expect) (6502::parse-code "lsr a")))))

(deftest parse-operand-variable
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value "some_label"
                 :address-mode '(6502::relative 6502::absolute 6502::zero-page))))
    (is (equalp (list expect) (6502::parse-code "lda some_label")))))

(deftest parse-operand-indexing
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value 8194
                 :address-mode '(6502::absolute-x 6502::zero-page-x))))
    (is (equalp (list expect) (6502::parse-code "lda $2002,x")))))

(deftest parse-operand-add-addresses
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value '(+ 700 2)
                 :address-mode '(6502::relative 6502::absolute 6502::zero-page))))
    (is (equalp (list expect) (6502::parse-code "lda 700+2")))))

(deftest parse-operand-add-variable
  (let ((expect (6502::make-instruction
                 :opcode :lda
                 :value '(+ "some_label" 2)
                 :address-mode '(6502::relative 6502::absolute 6502::zero-page))))
    (is (equalp (list expect) (6502::parse-code "lda some_label+2")))))

(deftest parse-operand-label-indexing
  (let ((expect (6502::make-instruction
                 :opcode :sta
                 :value "some_label"
                 :address-mode '(6502::absolute-x 6502::zero-page-x))))
    (is (equalp (list expect) (6502::parse-code "sta some_label,x")))))

(deftest parse-operand-label-indirect-ptr
  (let ((expect (6502::make-instruction
                 :opcode :sta
                 :value "memory_ptr"
                 :address-mode '(6502::indirect))))
    (is (equalp (list expect) (6502::parse-code "sta (memory_ptr)")))))

(deftest parse-operand-label-indirect-indexing
  (let ((expect (6502::make-instruction
                 :opcode :sta
                 :value "memory_ptr"
                 :address-mode '(6502::indirect-y))))
    (is (equalp (list expect) (6502::parse-code "sta (memory_ptr),y")))))
