(defpackage :boa-editor (:use :cl :cl-ppcre :cl-lex :yacc :com.gigamonkeys.binary-data :com.gigamonkeys.id3v2))
(in-package :boa-editor)

(defun add-earmuffs (symb)
  (intern (concatenate 'string "*" (symbol-name symb) "*")))

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
	(read-sequence seq stream)
	seq))

(defun copy-instance (i)
  (loop with i-class = (class-of i)
	 with c = (allocate-instance i-class)
	 for sd in (closer-mop:class-slots i-class)
	 for sn = (closer-mop:slot-definition-name sd)
	 when (slot-boundp i sn)
	 do (setf (slot-value c sn)
			  (slot-value i sn))
	 finally (return c)))

