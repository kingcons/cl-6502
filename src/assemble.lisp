(in-package :6502-cpu)

;;;; REFERENCES:
;; http://josephoswald.nfshost.com/comfy/summary.html
;; http://ahefner.livejournal.com/20528.html
;; https://github.com/mnaberez/py65/blob/master/src/py65/assembler.py

(defclass context ()
  ((symbol-table :initform (make-hash-table :test #'equal) :accessor asm-table)
   (bytevector :initform (make-array 32 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0)
               :accessor asm-array)))

(defvar *context* (make-instance 'context)
  "A context for the assembler to use.")

(defun extract-code (src)
  "Trim whitespace and remove comments from a given line of code, SRC."
  (flet ((trim-whitespace (str)
           (string-trim '(#\Tab #\Return #\Space) str)))
    (let ((trimmed-src (trim-whitespace src)))
      (trim-whitespace (subseq trimmed-src 0 (position #\; trimmed-src))))))

(defun asm (source)
  "Assemble SOURCE string into a bytevector to be loaded into memory and run."
  (concatenate 'vector (loop for line in (cl-ppcre:split "\\n" source)
                          collecting (asm-instruction line))))

(defun asm-instruction (code)
  "Assemble CODE into a bytevector, using the *CONTEXT* as necessary."
  nil)
