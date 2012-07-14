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

(defun extract-num (str mode)
  "Extract a hex number from its containing string, STR."
  (let ((token (cl-ppcre:scan-to-strings "[0-9a-fA-F]{2,4}" str)))
    (flet ((parse-hex (x) (parse-integer x :radix 16)))
      (when token
        (if (member mode '(absolute absolute-x absolute-y indirect))
            (mapcar #'parse-hex (list (subseq token 2) (subseq token 0 2)))
            (parse-hex token))))))

(defun asm (source)
  "Assemble SOURCE string into a bytevector and return it."
  (flet ((bytes (line)
           (if (or (position #\: line) (position #\= line))
               0
               (length (cl-ppcre:split "\\ " line)))))
    (apply 'concatenate 'vector
           (loop for line in (mapcar #'extract-code (cl-ppcre:split "\\n" source))
              for pc = 0 then (+ pc (bytes line))
              collecting (asm-instruction line pc)))))

(defun asm-instruction (code pc)
  "Assemble CODE into a bytevector, using the *CONTEXT* as necessary."
  (print pc)
  (let ((label-p (position #\: code))
        (store-p (position #\= code))
        (tokens (cl-ppcre:split "\\ " code)))
    (flet ((set-var (delimiter &optional default)
             (destructuring-bind (var &rest src) (cl-ppcre:split delimiter code)
               (setf (gethash var *symbol-table*) (or default (first src)))) nil))
      (cond (label-p (set-var "\\:" (format nil "$~4,'0x" pc)))
            (store-p (set-var "\\="))
            (t (process-tokens tokens))))))

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

(defun match-mode (str)
  "Given a string, STR, match it to an addressing mode returning that or NIL."
  (let ((regex-modes '(("^$" . implied)
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
                       ("^&[0-9a-fA-F]{2,4}$" . relative))))
    (loop for (regex . mode) in regex-modes
       when (cl-ppcre:scan regex str)
       return mode)))

(defun find-mode (tokens)
  "Determine which mode is being used by TOKENS, raising UNKNOWN-MODE otherwise."
  (let* ((token-str (apply 'concatenate 'string tokens))
         (result (if (gethash token-str *symbol-table*)
                     (match-mode (gethash token-str *symbol-table*))
                     (match-mode token-str))))
    (or result (error 'unknown-mode :tokens tokens))))

(defun process-tokens (tokens)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let* ((mnemonic (first tokens))
         (mode (find-mode (rest tokens)))
         (args (loop for token in (rest tokens)
                  for sym = (gethash token *symbol-table*)
                  for arg = (extract-num token mode)
                  if sym collect (extract-num sym mode)
                  else if arg collect arg
                  else unless (cl-ppcre:scan "^[aAxXyY]" token)
                  return (error 'syntax-error :token token
                                :line (format nil "~{~A ~}" tokens)))))
    (if (and mnemonic (not (gethash mnemonic *symbol-table*)))
        (apply 'vector (find-opcode mnemonic mode) (flatten args))
        #())))
