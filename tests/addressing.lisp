(in-package :6502-tests)

(def-suite addressing :in 6502-tests)
(in-suite addressing)

(deftest implied
    "The implied addressing mode is basically just a stub to remove style
warnings during macroexpansion of defopcode and friends."
  (is (null (implied cpu))))

(deftest accumulator
    "The accumulator mode is just a setf-able wrapper of the AR field in the
CPU structure."
  (setf (accumulator cpu) #x20)
  (is (= (cpu-ar cpu) (accumulator cpu) #x20)))

(deftest immediate
    "The immediate mode is just a setf-able wrapper of the PC field in the
CPU structure."
  (setf (immediate cpu) #x80)
  (is (= (cpu-pc cpu) (immediate cpu) #x80)))

(deftest zero-page
    "The zero-page mode should always return the (unsigned-byte 8) pointed to
by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (typep (zero-page cpu) '(unsigned-byte 8)))
    (is (= (get-byte pc-addr) (zero-page cpu)))))

(deftest zero-page-x
    "The zero-page-x mode should always return the (unsigned-byte 8) pointed
to by the Program Counter, summed with the X register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (x-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-xr cpu) x-reg)
    (is (typep (zero-page-x cpu) '(unsigned-byte 8)))
    (is (= (wrap-byte (+ x-reg (get-byte pc-addr))) (zero-page-x cpu)))))

(deftest zero-page-y
    "The zero-page-x mode should always return the (unsigned-byte 8) pointed
to by the Program Counter, summed with the Y register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (y-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-yr cpu) y-reg)
    (is (typep (zero-page-y cpu) '(unsigned-byte 8)))
    (is (= (wrap-byte (+ y-reg (get-byte pc-addr))) (zero-page-y cpu)))))

(deftest absolute
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (typep (absolute cpu) '(unsigned-byte 16)))
    (is (= (get-word pc-addr) (absolute cpu)))))

(deftest absolute-x
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter, summed with the X register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (x-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-xr cpu) x-reg)
    (is (typep (absolute-x cpu) '(unsigned-byte 16)))
    (is (= (absolute-x cpu) (wrap-word (+ (get-word pc-addr) x-reg))))))

(deftest absolute-y
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter, summed with the Y register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (y-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-yr cpu) y-reg)
    (is (typep (absolute-y cpu) '(unsigned-byte 16)))
    (is (= (absolute-y cpu) (wrap-word (+ (get-word pc-addr) y-reg))))))

(deftest indirect
    "The indirect mode should always return the (unsigned-byte 16) pointed to
by the word pointed to by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (typep (indirect cpu) '(unsigned-byte 16)))
    (is (= (indirect cpu) (get-word (get-word pc-addr))))))

(deftest indirect-x
    ""
  )

(deftest indirect-y
    ""
  )

(deftest relative
    ""
  )

(deftest defaddress
    "Defaddress creates both setf functions and address methods for the body
and name described."
  (6502::defaddress xreg (:cpu-reg t) (cpu-xr cpu))
  (6502::defaddress foo () 256)
  (setf (foo cpu) 8)
  (setf (xreg cpu) 9)
  (is (= 8 (get-byte 256)))
  (is (= 9 (cpu-xr cpu)))
  (is (= 17 (+ (xreg cpu) (get-byte (foo cpu))))))
