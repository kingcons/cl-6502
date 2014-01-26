(in-package :6502)

(defconstant +relative-branch-size-byte+ 2)
(defconstant +max-byte+ 256)

(defgeneric asm (source &optional init-env org-start)
  (:documentation "Assemble SOURCE into a bytevector and return it."))

(defmethod asm ((source list) &optional init-env org-start)
  (assemble-code-block (list-to-code-blocks source) init-env org-start))

(defmethod asm ((source string) &optional init-env org-start)
  (assemble-code-block (parse-code source) init-env org-start))

;;; Code-blocks are represented in an intermediary format - a list of
;;; association lists, one alist per line of source code. The keys are all
;;; optional, and include the following:
;;;  :label        A label for the current line, as a string.
;;;  :opcode       The opcode name, as a keyword.
;;;  :address-mode One of :immediate, :relative, :absolute, :zero-page,
;;;                :indirect, or :accumulator.
;;;  :value        The value of the operand, as an integer, or string if the
;;;                operand is a label, or tree if it's an expression.
;;;  :indexing     One of :x or :y.
;;; See tests/parser.lisp for example code-blocks created by the parser.

(defun list-to-code-blocks (list)
  "Given a list of assembly tuples, convert to it to alists."
  (unless (listp (first list))
    (setf list (list list)))
  (loop for tuple in list
       collect (let ((opcode (first tuple))
                     (value (second tuple))
                     address-mode indexing)
                 (when value
                   ; Convert operand.
                   (setf value (symbol-name value))
                   (when (or (string= value "a") (string= value "A"))
                     ; Operand is accumulator.
                     (setf address-mode :accumulator)
                     (setf value "00"))
                   (when (char= (aref value 0) #\#)
                     ; Operand is an immediate.
                     (setf address-mode :immediate)
                     (setf value (subseq value 1)))
                   (when (char= (aref value 0) #\@)
                     ; Operand is indirect addressing.
                     (setf address-mode :indirect)
                     (setf value (concatenate 'string "$" (subseq value 1))))
                   (when (char= (aref value 0) #\&)
                     ; Operand is relative addressing.
                     (setf address-mode :relative)
                     (setf value (concatenate 'string "$" (subseq value 1))))
                   (when (string= (subseq value (- (length value) 2)) ".X")
                     ; Operand uses x indexing.
                     (setf indexing :x)
                     (setf value (subseq value 0 (- (length value) 2))))
                   (when (string= (subseq value (- (length value) 2)) ".Y")
                     ; Operand uses y indexing.
                     (setf indexing :y)
                     (setf value (subseq value 0 (- (length value) 2))))
                   (when (common-lisp:and (> (length value) 3)
                                          (not address-mode))
                     ; Operand is absolute addressing.
                     (setf address-mode :absolute))
                   ; Handle hexidecimal or decimal.
                   (if (char= (aref value 0) #\$)
                     (setf value (parse-integer (subseq value 1) :radix 16))
                     (setf value (parse-integer value :radix 10))))
                 (make-alist :value value :address-mode address-mode
                             :opcode opcode :indexing indexing))))

(defmacro resolve-byte (place env)
  "Given a place and an environment, if that place is a function, call that
   function with the environment and assign the result to the place."
  (let ((byte-name (gensym)))
    `(when (functionp ,place)
       (let ((,byte-name (funcall ,place ,env)))
         (setf ,place ,byte-name)))))

(defun assemble-code-block (code-block &optional init-env org-start)
  "Given a list of alists, representing parsed source code, assemble each to a
   byte vector."
  (let ((labels-environment (or init-env (make-hash-table :test 'equal)))
        (assembled-bytes (make-array 0 :fill-pointer 0 :adjustable t))
        (pc-start (if org-start org-start 0)))
    ; Build byte vector, without labels.
    (loop for code-line in code-block
       do (let ((bytes (assemble-code-line code-line
                                           (+ pc-start (length assembled-bytes))
                                           labels-environment)))
            (loop for b in bytes
                 do (vector-push-extend b assembled-bytes))))
    ; Resolve labels in the byte vector.
    (loop for i from 0 below (length assembled-bytes)
         do (resolve-byte (aref assembled-bytes i) labels-environment))
    assembled-bytes))

(defun assemble-code-line (code-line pc labels-environment)
  "Given an alist representing a line of code, and the current program counter,
   fill the labels environment with any labels and assemble code to a list of
   bytes."
  (let ((opcode (cdr (assoc :opcode code-line)))
        (value (cdr (assoc :value code-line)))
        (label (cdr (assoc :label code-line)))
        address-mode)
    (when label
      (setf (gethash label labels-environment) pc))
    (when opcode
      (setf address-mode (resolve-address-mode code-line labels-environment))
      (append (get-binary-opcode opcode address-mode)
              (get-binary-operand value address-mode pc)))))

(defun match-opcode-data (data opcode &optional address-mode)
  "Returns whether the asm metadata matches the given opcode, and address-mode
   if it is provided."
  (common-lisp:and (eq (first data) (intern (symbol-name opcode) :6502))
      (if address-mode
          (eq (fifth data) (intern (symbol-name address-mode) :6502))
          t)))

(defun get-binary-opcode (opcode address-mode)
  "Returns the numerical value for the given opcode and address-mode."
  (list (position-if #'(lambda (e) (match-opcode-data e opcode address-mode))
                     *opcode-meta*)))

(defun get-binary-operand (value address-mode pc)
  "Given the operand, address-mode, and program counter, return a list of
   assembled bytes, using delayed functions for labels or expressions."
  (case address-mode
    ((absolute absolute-x absolute-y indirect)
     (list (make-byte value pc :low)
           (make-byte value pc :high)))
    (implied nil)
    (accumulator nil)
    (relative (list (make-byte value pc :relative)))
    (otherwise (list (make-byte value pc :low)))))

(defun make-byte (value pc type)
  "Given an integer, return a single byte for the required type. Given a label,
   return a delayed function to calculate the same, once labels are defined."
  (cond
    ((stringp value)
     (lambda (labels-environment)
       (let ((addr (gethash value labels-environment)))
         (unless addr (error (format nil "Undefined label ~s" value)))
         (when (eq type :relative)
           (setf addr (- addr pc +relative-branch-size-byte+)))
         (make-byte addr pc type))))
    ((common-lisp:and (listp value) (eq (car value) '+))
     (lambda (labels-environment)
       (destructuring-bind (unused-plus operand-1 operand-2) value
         (let ((value-1 (make-byte operand-1 pc type))
               (value-2 (make-byte operand-2 pc type)))
           (resolve-byte value-1 labels-environment)
           (resolve-byte value-2 labels-environment)
           (+ value-1 value-2)))))
    ((numberp value)
     (if (eq type :high)
         (floor (/ value +max-byte+))
         (mod value +max-byte+)))
    (t (error (format nil "Cannot make-byte from ~s" value)))))

(defun get-possible-address-modes (opcode)
  "Given an opcode, return the possible address modes for that operation."
  (loop for e across *opcode-meta*
       when (match-opcode-data e opcode)
       collect (intern (symbol-name (fifth e)) :keyword)))

(defun upgrade-indexing (mode indexing)
  "Given an adress mode and type of indexing, combine them and return the
   combined address mode."
  (case indexing
    (:x (intern (concatenate 'string (symbol-name mode) "-X") :6502))
    (:y (intern (concatenate 'string (symbol-name mode) "-Y") :6502))
    ((nil) mode)
    (otherwise (error (format nil "Invalid indexing: ~s" indexing)))))

(defun zero-page-address (addr labels-environment)
  "Returns whether the address is a zero page access."
  (cond
    ((numberp addr) (< addr +max-byte+))
    ((stringp addr)
     (let ((addr (gethash addr labels-environment)))
       (common-lisp:and (numberp addr) (< addr +max-byte+))))
    ((listp addr) nil)
    (t (error (format nil "Invalid address ~s" addr)))))

(defun is-accumulator (text)
  "Returns whether the operand is a reference to the accumulator."
  (common-lisp:and (stringp text)
                   (or (string= text "a") (string= text "A"))))

(defun resolve-address-mode (code-line labels-environment)
  "Given an alist representing a line of code, get the address mode that the
   code is using."
  (let ((value (cdr (assoc :value code-line)))
        (opcode (cdr (assoc :opcode code-line)))
        (indexing (cdr (assoc :indexing code-line)))
        (address-mode (cdr (assoc :address-mode code-line))))
    (unless value
      (return-from resolve-address-mode 'implied))
    (case address-mode
      (:relative 'relative)
      (:immediate 'immediate)
      (:accumulator 'accumulator)
      (:indirect (upgrade-indexing 'indirect indexing))
      (:absolute (upgrade-indexing 'absolute indexing))
      (:zero-page (upgrade-indexing 'zero-page indexing))
      ((nil) (let ((possible-modes (get-possible-address-modes opcode)))
               (cond
                 ((common-lisp:and (find :accumulator possible-modes)
                                   (is-accumulator value)) 'accumulator)
                 ((common-lisp:and (find :zero-page possible-modes)
                                   (zero-page-address value labels-environment))
                  (upgrade-indexing 'zero-page indexing))
                 ((find :absolute possible-modes)
                  (upgrade-indexing 'absolute indexing))
                 ((find :relative possible-modes) 'relative)
                 (t (error (format nil "Unknown mode ~s" possible-modes))))))
      (otherwise (error (format nil "Unknown mode ~s" code-line))))))
