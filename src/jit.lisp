(in-package :6502)

(defvar *jit-cache* (make-hash-table)
  "The JIT's hot code cache. Currently never invalidated.")

(defun get-basic-block (cpu)
  "Get the opcodes from the current PC to the next branch."
  (let ((control-flow-ops '(bcc bcs beq bmi bne bpl bvc bvs
                            brk jmp jsr rti rts)))
    (loop for pc = (cpu-pc cpu) then (+ pc op-length)
       for op = (get-byte pc)
       for (op-name nil nil op-length) = (aref *opcode-meta* op)
       collect op until (member op-name control-flow-ops))))

(defun jit-block (opcodes)
  "Given a list of opcodes, JIT compile an equivalent function."
  (flet ((jit-op (x) `(funcall ,(aref *opcode-funs* x) cpu)))
    (compile nil `(lambda (cpu) ,@(mapcar #'jit-op opcodes)))))

(defun jit-step (cpu pc)
  "If the current block has been JIT compiled, run it, otherwise JIT compile it."
  (alexandria:if-let (fn (gethash pc *jit-cache*))
    (funcall fn cpu)
    (let ((code (jit-block (get-basic-block cpu))))
      (setf (gethash pc *jit-cache*) code)
      (funcall code cpu))))
