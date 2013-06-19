(in-package :6502)

(defvar *jit-cache* (make-hash-table)
  "The JIT's hot code cache. Currently never invalidated.")

(defun get-basic-block (cpu)
  "Get the opcodes from the current PC to the next branch."
  (flet ((op-length (x) (fourth (aref *opcode-meta* x))))
    (loop for pc = (cpu-pc cpu) then (+ pc (op-length op))
       for op = (get-byte pc) collect op
       until (member op '(#x90 #xb0 #xf0 #x30 #xd0 #x10 #x50 #x70
                          #x00 #x4c #x6c #x20 #x40 #x60)))))

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
