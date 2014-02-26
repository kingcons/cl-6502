(in-package :6502)

(defconstant +relative-branch-size-byte+ 2)
(defconstant +max-byte+ 256)
(defparameter +absolute-modes+ '(absolute absolute-x absolute-y))
(defparameter +zero-page-modes+ '(zero-page zero-page-x zero-page-y))

(defgeneric asm (source &optional init-env org-start)
  (:documentation "Assemble SOURCE into a bytevector and return it."))

(defmethod asm ((source list) &optional init-env org-start)
  (assemble-code-block (list-to-instructions source) init-env org-start))

(defmethod asm ((source string) &optional init-env org-start)
  (assemble-code-block (parse-code source) init-env org-start))

(defstruct instruction
  "Represents a single line of code."
  (label        nil :type (or null string))
  (opcode       nil :type (or null symbol))
  (address-mode nil :type (or null symbol list))
  (value        nil :type (or null u16 list string)))

(defun list-to-instructions (instructions)
  "Given a list of assembly tuples, convert them to instructions."
  (unless (listp (first instructions))
    (setf instructions (list instructions)))
  (loop for tuple in instructions collect (apply #'tuple-to-instruction tuple)))

(defun tuple-to-instruction (opcode &optional operand)
  "Given an opcode and value, as symbols, convert them to an instruction."
  (unless operand
    (return-from tuple-to-instruction
      (make-instruction :opcode opcode :address-mode 'implied)))
  (let ((token (transform-sexp-syntax operand)))
    (destructuring-bind (possible-modes value-start value-end)
        (operand-possible-modes-and-value token)
      (declare (ignore end))
      (let ((stream (make-stream (coerce (subseq token value-start value-end)
                                         '(vector character)))))
        (make-instruction :opcode opcode :value (fetch-literal stream)
                          :address-mode possible-modes)))))

(defun transform-sexp-syntax (sexp-token)
  "Given a SEXP-token using an indirect, *.x or *.y addressing mode, transform
   it to use the classic string assembly syntax."
  (substitute #\, #\. (cl-ppcre:regex-replace "\@([^.]*)(.*)?"
                                              (string sexp-token) "($\\1)\\2")))

(defmacro resolve-byte (place env)
  "Given a place and an environment, if that place is a function, call that
   function with the environment and assign the result to the place."
  (let ((byte-name (gensym)))
    `(when (functionp ,place)
       (let ((,byte-name (funcall ,place ,env)))
         (setf ,place ,byte-name)))))

(defun assemble-code-block (code-block &optional init-env org-start)
  "Given a list of instructions, assemble each to a byte vector."
  (let ((env (or init-env (make-hash-table :test 'equal)))
        (output (make-array 0 :fill-pointer 0 :adjustable t))
        (pc-start (or org-start 0)))
    ; Build byte vector, without labels.
    (loop for instruction in code-block
       do (let ((bytes (assemble-instruction instruction
                                             (+ pc-start (length output)) env)))
            (loop for b in bytes
               do (vector-push-extend b output))))
    ; Resolve labels in the byte vector.
    (loop for i from 0 below (length output)
       do (resolve-byte (aref output i) env))
    output))

(defun assemble-instruction (instruction pc env)
  "Given an instruction, and the current program counter, fill the environment
   with any labels and assemble instruction to a list of bytes."
  (with-slots (opcode value label) instruction
    (when label
      (setf (gethash label env) pc))
    (when opcode
      (let ((mode (decide-address-mode instruction env)))
        (list* (find-opcode opcode mode) (process-args value mode pc))))))

(defun find-opcode (opcode mode)
  "Finds an opcode matching OPCODE and MODE, raising ILLEGAL-OPCODE otherwise."
  (let ((match (position-if #'(lambda (e) (match-opcode-data e opcode mode))
                            *opcode-meta*)))
    (or match (error 'illegal-opcode :opcode (list opcode mode)))))

(defun process-args (value address-mode pc)
  "Given the operand value, address-mode, and program counter, return a list of
   assembled bytes, using delayed functions for labels or expressions."
  (case address-mode
    ((absolute absolute-x absolute-y indirect)
     (list (make-byte value pc :low) (make-byte value pc :high)))
    ((implied accumulator) nil)
    (relative (list (make-byte value pc :relative)))
    (otherwise (list (make-byte value pc :low)))))

(defun decide-address-mode (instruction env)
  "Finds the desired address mode, matching what the opcode allows to what was
   parsed from the operand's syntax."
  (with-slots (opcode address-mode value) instruction
    (let ((modes (if (listp address-mode) address-mode (list address-mode)))
          (opcode-modes (get-opcode-address-modes opcode)))
      (if (common-lisp:and (zero-page-address value env)
               (intersection opcode-modes +zero-page-modes+))
          (setf modes (set-difference modes +absolute-modes+))
          (setf modes (set-difference modes +zero-page-modes+)))
      (first (intersection modes opcode-modes)))))

(defun get-opcode-address-modes (opcode)
  "Given an opcode, return the possible address modes for that operation."
  (loop for e across *opcode-meta*
     when (match-opcode-data e opcode :any) collect (fifth e)))

(defun match-opcode-data (data opcode &optional (address-mode :any))
  "Returns whether the asm metadata matches the given opcode, and address-mode
   if it is provided."
  (common-lisp:and (eq (first data) (intern (symbol-name opcode) :6502))
                   (or (eq address-mode :any) (eq address-mode (fifth data)))))

(defun zero-page-address (addr env)
  "Returns whether the address is a zero page access."
  (cond
    ((numberp addr) (< addr +max-byte+))
    ((stringp addr)
     (let ((addr (gethash addr env)))
       (common-lisp:and (numberp addr) (< addr +max-byte+))))
    ((listp addr) nil)
    (t (error "Invalid address" :argument addr))))

(defun make-byte (value pc type)
  "Given an integer, return a single byte for the required type. Given a label,
   return a delayed function to calculate the same, once labels are defined."
  (cond
    ((stringp value)
     (lambda (env)
       (let ((addr (or (gethash value env)
                       (error "Undefined label" :argument value))))
         (when (eq type :relative)
           (setf addr (- addr pc +relative-branch-size-byte+)))
         (make-byte addr pc type))))
    ((common-lisp:and (listp value) (eq (first value) '+))
     (lambda (env)
       (destructuring-bind (unused-plus operand-1 operand-2) value
         (declare (ignore unused-plus))
         (+ (make-and-resolve-byte operand-1 pc type env)
            (make-and-resolve-byte operand-2 pc type env)))))
    ((numberp value)
     (if (eq type :high) (floor (/ value +max-byte+)) (mod value +max-byte+)))
    (t (error "Cannot make-byte" :argument value))))

(defun make-and-resolve-byte (operand pc type env)
  "Given an operand, convert it to a byte, resolving any delayed functions."
  (let ((value (make-byte operand pc type)))
    (resolve-byte value env)
    value))
