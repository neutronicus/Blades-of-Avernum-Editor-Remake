(in-package :boa-editor)

;; Read entire contents of a text file into a string - do this so that cl-ppcre can effectively
;; search global.h for data definitions.

;; These are the structures that the BoA scenario editor writes to disk.  They are stored
;; in the following order:
;; scenario-data-type ++ (vector outdoor-record-type)
;; ++ (vector (town-record-type ++ [tiny-tr-type OR ave-tr-type OR big-tr-type]))
(defmacro define-boa-datastructures (names)
  (let ((names (map 'list
					#'(lambda (n)
						(regex-replace-all "-" (string-downcase (symbol-name n)) "_"))
					names)))
	`(,@(parse-boa-datastructures names "/Users/adamvh/Documents/Programming/Lisp/BOAEditorRemake/global.h"))))

(defmacro make-boa-defines (filename)
  (with-open-file (s filename)
	`(progn
	   ,@(loop
		  with scanner = (create-scanner '(:sequence "#define"
										   (:greedy-repetition 1 nil :whitespace-char-class)
										   (:register (:greedy-repetition 1 nil :word-char-class))
										   (:greedy-repetition 1 nil :whitespace-char-class)
										   (:register (:greedy-repetition 1 nil :digit-class))) :single-line-mode t)
		   with defs = ()
		   for line = (read-line s nil nil)
		  while line
		  do (multiple-value-bind (match-start match-end register-starts register-ends)
				 (scan scanner line)
			   (declare (ignorable match-end))
			   (if match-start
				   (progn
					 (print line)
					 (push
					  `(defvar ,(intern
								 (concatenate 'string "*"
											  (regex-replace-all "_" (subseq line (aref register-starts 0) (aref register-ends 0)) "-")
											  "*"))
						 ,(read-from-string (subseq line (aref register-starts 1) (aref register-ends 1))))
					  defs))))
		   finally (return defs)))))

;; This function actually crawls through global.h, finding definitions for all requested types,
;; as well as the types those types depend on.  Should only be called at compile time.
(defun parse-boa-datastructures (names filename)
	  (let* ((g!file (open filename))
			 (g!file-contents (slurp-stream g!file))
			 (g!stack nil)
			 (g!known-types nil))
		(labels
			((g!parse-struct (name)
			   (progn
				 (let ((g!struct-finder
						(create-scanner
						 `(:alternation
						   (:sequence "class" (:greedy-repetition 1 nil :whitespace-char-class)
									  ,name (:greedy-repetition 1 nil :whitespace-char-class)
									  "{" (:non-greedy-repetition 0 nil (:inverted-char-class #\})) "}"
									  (:greedy-repetition 0 nil :whitespace-char-class) ";")
						   (:sequence "typedef" (:greedy-repetition 1 nil :whitespace-char-class)
									  "struct" (:greedy-repetition 0 nil :whitespace-char-class)
									  "{" (:non-greedy-repetition 0 nil (:inverted-char-class #\})) "}"
									  (:greedy-repetition 0 nil :whitespace-char-class) ,name
									  ";")) :single-line-mode t)))
				   (format t "name: ~a~%" name)
				   (multiple-value-bind (g!start g!end)
					   (scan g!struct-finder g!file-contents)
					 (let* ((parsed-struct (parse-with-lexer (g!lexer g!file-contents :start g!start :end g!end)
															 c-struct-parser))
							(name (car parsed-struct))
							(data (cadr parsed-struct)))
					   (progn
						 (push `(define-binary-class ,(intern (concatenate 'string (symbol-name name) "-LITTLE-ENDIAN")) ()
								  ,(map 'list #'(lambda (d) (g!find-type d :little-endian)) data))
							   g!stack)
						 (push `(define-binary-class ,(intern (concatenate 'string (symbol-name name) "-BIG-ENDIAN")) ()
								  ,(map 'list #'(lambda (d) (g!find-type d :big-endian)) data))
							   g!stack)))))))
			 (g!find-type (declaration endian-ness)
			   (labels ((type-converter (type-spec)
						  (ecase (length type-spec)
							(1 (case (car type-spec)
								 (short 's2)
								 (char 'u1)
								 (boolean 's1)
								 (otherwise
								  (progn
									(unless (member (car type-spec) g!known-types)
									  (g!parse-struct
									   (regex-replace-all "-" (string-downcase (symbol-name (car type-spec))) "_")))
									(push (car type-spec) g!known-types)
									(intern (concatenate 'string (symbol-name (car type-spec)) (ecase endian-ness
																								 (:little-endian "-LITTLE-ENDIAN")
																								 (:big-endian "-BIG-ENDIAN"))))))))
							(2 (case (cadr type-spec)
								 (short 'u2)
								 (int 'u4)
								 (char 'u1)
								 (otherwise
								  (progn
									(unless (member (cadr type-spec) g!known-types)
									  (g!parse-struct
									   (regex-replace-all "-" (string-downcase (symbol-name (cadr type-spec))) "_")))
									(push (cadr type-spec) g!known-types)
									(intern (concatenate 'string (symbol-name (cadr type-spec)) (ecase endian-ness
																								  (:little-endian "-LITTLE-ENDIAN")
																								  (:big-endian "-BIG-ENDIAN")))))))))))
				 (let ((declaration (reverse declaration)))
				   (if (consp (car declaration))
					   (let ((dimension (car declaration))
							 (name (cadr declaration))
							 (type (type-converter (reverse (cddr declaration)))))
						 (if (member type '(s1 s2 u1 u2))
							 `(,(case name
									  (floor 'floor-type)
									  (number 'creature-number)
									  (special 'special-type)
									  (otherwise name))
								,(binary-vector type dimension :endian-ness endian-ness))
							 `(,(case name
									  (floor 'floor-type)
									  (number 'creature-number)
									  (special 'special-type)
									  (otherwise name))
								,(binary-vector type dimension))))
					   (let ((name (car declaration))
							 (type (type-converter (reverse (cdr declaration)))))
						 (if (member type '(u2 u4 u1 s1 s2 s4))
							 `(,(case name
									  (floor 'floor-type)
									  (number 'creature-number)
									  (special 'special-type)
									  (otherwise name))
								(,type :endian-ness ,endian-ness))
							 `(,(case name
									  (floor 'floor-type)
									  (number 'creature-number)
									  (special 'special-type)
									  (otherwise name))
								,type))))))))
		  (progn (loop for name in names doing (g!parse-struct name))
				 `(progn
					,@(loop for class-definition in g!stack collecting class-definition))))))