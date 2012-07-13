(in-package :6502-cpu)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defvar *symbol-table* (make-hash-table :test #'equal)
  "A symbol-table for use during assembly.")

(defun trim-whitespace (str)
  "Remove trailing and leading Tabs, Returns, and Spaces from STR."
  (string-trim '(#\Tab #\Return #\Space) str))

(defun extract-code (src)
  "Trim whitespace and remove comments from a given line of code, SRC."
  (let ((trimmed-src (trim-whitespace src)))
    (trim-whitespace (subseq trimmed-src 0 (position #\; trimmed-src)))))

(defun asm (source)
  "Assemble SOURCE string into a bytevector and return it."
  (apply 'concatenate 'vector
         (loop for line in (cl-ppcre:split "\\n" source)
            collecting (asm-instruction (extract-code line)))))

(defun asm-instruction (code)
  "Assemble CODE into a bytevector, using the *CONTEXT* as necessary."
  (let ((label-p (position #\: code))
        (store-p (position #\= code))
        (tokens (cl-ppcre:split "\\ " code)))
    (flet ((set-var (delimiter &optional default)
             (destructuring-bind (var val &rest src)
                 (cl-ppcre:split delimiter code)
               (setf (gethash var *symbol-table*) (or default val))
               (setf tokens (cl-ppcre:split "\\ " src)))))
      ; TODO: Track 'Program Counter' for Labels. Will need to be done in ASM.
      (cond (label-p (set-var "\\:" 0))
            (store-p (set-var "\\=")))
      (process-tokens tokens))))

(defun find-opcode (name mode)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise."
  (flet ((match-p (op)
           (and (string-equal name (symbol-name (first op)))
                (eql mode (second (alexandria:lastcar op))))))
    (let ((result (loop for i from 0 to 255
                     when (match-p (aref *opcodes* i))
                     collect i)))
      (unless result
        (error 'illegal-opcode :opcode (format nil "Name:~A Mode:~A" name mode)))
      (first result))))

(defun find-mode (tokens)
  "Determine which mode is being used by TOKENS, raising UNKNOWN-MODE otherwise."
  (let* ((token-str (apply 'concatenate 'string tokens))
         (regex-modes '(("^$" . implied)
                        ("^[aA]$" . accumulator)
                        ("^#\\$[0-9a-fA-F]{2}$" . immediate)
                        ("^\\$[0-9a-fA-F]{4},[xX]$" . absolute-x)
                        ("^\\$[0-9a-fA-F]{4},[yY]$" . absolute-y)
                        ("^\\$[0-9a-fA-F]{4}$" . absolute)
                        ("^\\$[0-9a-fA-F]{2},[xX]$" . zero-page-x)
                        ("^\\$[0-9a-fA-F]{2},[yY]$" . zero-page-y)
                        ("^\\$[0-9a-fA-F]{2}$" . zero-page)
                        ("^\\(\\$[0-9a-fA-F]{2}\\),[xX]$" . indirect-x)
                        ("^\\(\\$[0-9a-fA-F]{2}\\),[yY]$" . indirect-y)
                        ("^\\(\\$[0-9a-fA-F]{4}\\)$" . indirect)
                        ("^&[0-9a-fA-F]{2,4}$" . relative)))
         (result (loop for (regex . mode) in regex-modes
                    when (cl-ppcre:scan regex token-str)
                    return mode)))
    (unless result
      (error 'unknown-mode :tokens tokens))
    result))

(defun process-tokens (tokens)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let* ((mnemonic (first tokens))
         (mode (find-mode (rest tokens)))
         (args (loop for token in (rest tokens)
                  for arg = (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" token)
                  if (member mode '(absolute absolute-x absolute-y indirect))
                    return (mapcar (lambda (x) (parse-integer x :radix 16))
                                   (list (subseq arg 2) (subseq arg 0 2)))
                  if arg
                    collect (parse-integer arg :radix 16)
                  else unless (cl-ppcre:scan "^[aAxXyY]" token)
                    collect (or (gethash token *symbol-table*)
                                (error 'syntax-error :token token
                                       :line (format nil "~{~A ~}" tokens))))))
    (if mnemonic
        (concatenate 'vector (vector (find-opcode mnemonic mode)) args)
        #())))
