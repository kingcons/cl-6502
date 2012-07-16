(in-package :6502-cpu)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defvar *symtable* (make-hash-table :test #'equal)
  "A symbol table for use during assembly.")

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

(defun tokenize (line)
  "Split a LINE by spaces into its constituent tokens."
  (cl-ppcre:split "\\ " line))

(defun preprocess-p (line)
  "Take a LINE of source and return T if it contains a preprocessor statement."
  (or (position #\: line) (position #\= line)))

(defun preprocess (line pc)
  "Preprocess the line setting values in the *SYMTABLE* accordingly."
  (flet ((set-var (delimiter &optional val)
           (destructuring-bind (var &rest src) (cl-ppcre:split delimiter line)
             (setf (gethash var *symtable*) (or val (first src))))))
    (cond ((position #\: line) (set-var "\\:" (format nil "~4,'0x" pc)))
          ((position #\= line) (set-var "\\=")))))

(defun next-pc (tokens pc)
  "Compute the new PC from TOKENS and the old PC."
  (let* ((name (first tokens))
         (mode (and name (find-mode (process-syms (rest tokens) pc)))))
    (if name
        (+ pc (third (aref *opcodes* (find-opcode name mode))))
        pc)))

(defun asm (source)
  "Assemble SOURCE string into a bytevector and return it."
  (clrhash *symtable*)
  (apply 'concatenate 'vector
         (loop for line in (mapcar #'extract-code (cl-ppcre:split "\\n" source))
            for pc = 0 then (or (unless (preprocess-p line)
                                  (next-pc (tokenize line) pc)) pc)
            if (preprocess-p line) do (preprocess line pc)
            else collect (process-tokens (tokenize line) pc))))

(defun find-opcode (name mode)
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise."
  (flet ((match-p (op)
           (and (string-equal name (symbol-name (first op)))
                (eql mode (second (alexandria:lastcar op))))))
    (let ((result (loop for i from 0 to 255
                     when (match-p (aref *opcodes* i)) return i)))
      (or result (error 'illegal-opcode :opcode (list name mode))))))

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
       when (cl-ppcre:scan regex str) return mode)))

(defun find-mode (tokens)
  "Determine which mode is being used by TOKENS, raising UNKNOWN-MODE otherwise."
  (let ((result (match-mode (apply 'concatenate 'string tokens))))
    (or result (error 'unknown-mode :tokens tokens))))

(defun process-syms (tokens)
  "Resolve any symbols in TOKENS."
  (let ((prefixes '(#\# #\$ #\( #\&)))
    (labels ((prefix-sym (x)
               (subseq x 0 (1+ (loop for char in prefixes
                                  maximizing (or (position char x) 0)))))
             (splice-sym (x)
               (let ((result (gethash (string-trim prefixes x) *symtable*)))
                 (when result
                   (concatenate 'string (prefix-sym x) result)))))
      (mapcar (lambda (sym) (or (splice-sym sym) sym)) tokens))))

(defun process-args (tokens mode)
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  (flatten (remove nil (mapcar (lambda (x) (extract-num x mode)) tokens))))

(defun process-tokens (tokens)
  "Takes a tokenized block of code and generates an appropriate 6502 bytevector."
  (let* ((mnemonic (first tokens))
         (args (process-syms (rest tokens)))
         (mode (find-mode args)))
    (when tokens
      (apply 'vector (find-opcode mnemonic mode) (process-args args mode)))))
