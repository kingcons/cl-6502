(in-package :6502-tests)

(def-suite addressing :in 6502-tests)
(in-suite addressing)

(deftest implied
    "The implied addressing mode is basically just a stub to remove style
warnings during macroexpansion of defopcode and friends."
  (is (null (getter implied t))))

(deftest accumulator
    "The accumulator mode is just a setf-able wrapper of the AR field in the
CPU structure."
  (setter 'accumulator #x20)
  (is (= (cpu-ar cpu) (getter accumulator t) #x20)))

(deftest immediate
    "The immediate mode is just a setf-able wrapper of the PC field in the
CPU structure."
  (setter 'immediate #x80)
  (is (= (cpu-pc cpu) (getter immediate t) #x80)))

(deftest zero-page
    "The zero-page mode should always return the (unsigned-byte 8) pointed to
by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (= (get-byte pc-addr) (getter zero-page t)))))

(deftest zero-page-x
    "The zero-page-x mode should always return the (unsigned-byte 8) pointed
to by the Program Counter, summed with the X register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (x-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-xr cpu) x-reg)
    (is (= (wrap-byte (+ x-reg (get-byte pc-addr)))
           (getter zero-page-x t)))))

(deftest zero-page-y
    "The zero-page-x mode should always return the (unsigned-byte 8) pointed
to by the Program Counter, summed with the Y register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (y-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-yr cpu) y-reg)
    (is (= (wrap-byte (+ y-reg (get-byte pc-addr)))
           (getter zero-page-y t)))))

(deftest absolute
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (= (get-word pc-addr) (getter absolute t)))))

(deftest absolute-x
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter, summed with the X register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (x-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-xr cpu) x-reg)
    (is (= (wrap-word (+ (get-word pc-addr) x-reg))
           (getter absolute-x t)))))

(deftest absolute-y
    "The absolute mode should always return the (unsigned-byte 16) pointed to
by the Program Counter, summed with the Y register and wrapped."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff))
            (y-reg (gen-integer :min 0 :max #xff)))
    (setf (cpu-pc cpu) pc-addr (cpu-yr cpu) y-reg)
    (is (= (wrap-word (+ (get-word pc-addr) y-reg))
           (getter absolute-y t)))))

(deftest indirect
    "The indirect mode should always return the (unsigned-byte 16) pointed to
by the word pointed to by the Program Counter."
  (for-all ((pc-addr (gen-integer :min 0 :max #xffff)))
    (setf (cpu-pc cpu) pc-addr)
    (is (= (get-word (get-word pc-addr)) (getter indirect t)))))

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
  (setter foo 8)
  (setter xreg 9)
  (is (= 8 (get-byte 256)))
  (is (= 9 (cpu-xr cpu)))
  (is (= 17 (+ (getter xreg t)
               (get-byte (getter foo t))))))
