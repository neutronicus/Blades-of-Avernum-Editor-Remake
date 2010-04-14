(use-package :cl-lex)
(use-package :cl-ppcre)
(use-package :yacc)
(use-package :binary-data)
(use-package :id3v2)

(defmacro define-c-struct-lexer (name)
  `(cl-lex:define-string-lexer ,name
	 ("//.*[\\n\\r]+")
	 ,@(loop for symb in '(|;| |(| |)| |{| |}| |[| |]| |,| |:|)
		  collecting
		  `(,(concatenate 'string "\\" (string symb)) (return (values ',symb ',symb))))
	 ("class" (return (values 'class 'class)))
	 ("public" (return (values 'public 'public)))
	 ("typedef" (return (values 'typedef 'typedef)))
	 ("struct" (return (values 'struct 'struct)))
	 ("([A-Za-z]\\w*)" (return (values 'identifier (intern (string-upcase (regex-replace-all "_" $1 "-"))))))
	 ("(\\d+)" (return (values 'number (read-from-string $1))))))

(define-c-struct-lexer c-struct-lexer)

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun define-boa-datastructures (names filename &key (nflags 0))
  (let* ((g!file (open filename))
		 (g!file-contents (slurp-stream g!file))
		 (g!stack nil)
		 (g!known-types nil))
	(define-c-struct-lexer g!lexer)
	(labels
		((g!parse-struct (name &key (nflags 0) little-endian-test)
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
				   (if (> nflags 0)
					   (let ((flags (subseq data 0 nflags))
							 (rest-of-data (nthcdr nflags data))
							 (main-name (string-upcase (regex-replace-all "_" (symbol-name name) "-")))
							 (le-name (string-upcase (concatenate
													  'string (regex-replace-all "_" (symbol-name name) "-")
													  "-little-endian")))
							 (be-name (string-upcase (concatenate
													  'string (regex-replace-all "_" (symbol-name name) "-")
													  "-big-endian"))))
						 (push `(define-tagged-binary-class ,(intern main-name) ()
								  ,@(map 'list #'(lambda (d) (g!find-type d :little-endian)) flags)
								  (:dispatch (if ,little-endian-test
												 ,(intern le-name)
												 ,(intern be-name))))
							   g!stack)
						 (push `(define-binary-class ,(intern le-name) ()
								  ,@(map 'list #'(lambda (d) (g!find-type d :little-endian)) rest-of-data))
							   g!stack)
						 (push `(define-binary-class ,(intern be-name) ()
								  ,@(map 'list #'(lambda (d) (g!find-type d :big-endian)) rest-of-data))
							   g!stack)))
				   (progn
					 (push `(define-binary-class ,(intern (concatenate 'string (symbol-name name) "-LITTLE-ENDIAN")) ()
							  ,@(map 'list #'(lambda (d) (g!find-type d :little-endian)) data))
						   g!stack)
					 (push `(define-binary-class ,(intern (concatenate 'string (symbol-name name) "-BIG-ENDIAN")) ()
							  ,@(map 'list #'(lambda (d) (g!find-type d :big-endian)) data))
						   g!stack)))))))
		 (g!find-type (declaration endian-ness)
		   (labels ((type-converter (type-spec)
					  (ecase (length type-spec)
						(1 (case (car type-spec)
							 (short 's2)
							 (char 's1)
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
					 (if (member type '(s1 s2))
						 `(,name (binary-vector ',type ',dimension :endian-ness ,endian-ness))
						 `(,name (binary-vector ',type ',dimension))))
				   (let ((name (car declaration))
						 (type (type-converter (reverse (cdr declaration)))))
					 (if (member type '(u2 u4 u1 s1 s2 s4))
						 `(,name (,type :endian-ness ,endian-ness))
						 `(,name ,type))))))))
	  (progn (loop for name in names doing (g!parse-struct name :nflags nflags :little-endian-test `(and (= flag1 10) (=flag2 20) (=flag3 30) (=flag4 40))))
			 `(progn
				,@(loop for class-definition in g!stack collecting class-definition))))))

(define-parser c-struct-parser
  (:start-symbol record)
  (:terminals ({ } |(| |)| [ ] |;| |,| |:| identifier number class public typedef struct class))
  (record
   (typedef struct { declaration-list } identifier |;|
			#'(lambda (a b c d e f g) (declare (ignore a b c e g)) (list f (map 'list #'identity d))))
   (class identifier { public |:| declaration-list } |;|
		  #'(lambda (a b c d e f g h) (declare (ignore a c d e g h))
					(list b (map 'list #'identity f)))))
  (declaration-list
   (declaration |;| declaration-list #'(lambda (a b c) (declare (ignore b)) (cons a c)))
   (declaration |,| comma-declaration-list |;|
				#'(lambda (a b c d)
					(declare (ignore b d))
					(cons a
						  (concatenate 'list (map 'list #'(lambda (x) (concatenate 'list (subseq a 0 (if (consp (car (last a)))
																										 (- (length a) 2)
																										 (1- (length a)))) x)) c)
									   nil))))
   (declaration |,| comma-declaration-list |;| declaration-list
				#'(lambda (a b c d e)
					(declare (ignore b d))
					(cons a
						  (concatenate 'list (map 'list #'(lambda (x) (concatenate 'list (subseq a 0 (if (consp (car (last a)))
																										 (- (length a) 2)
																										 (1- (length a)))) x)) c)
									   e))))
   (declaration |;| #'(lambda (a b) (declare (ignore b)) (cons a nil)))
   (declaration |(| comma-declaration-list |)| |;| declaration-list #'(lambda (a b c d e f)
																		(declare (ignore a b c d e)) f))
   (declaration |(| |)| |;| declaration-list #'(lambda (a b c d e) (declare (ignore a b c d)) e)))
  (comma-declaration-list
   (declaration |,| comma-declaration-list #'(lambda (a b c) (declare (ignore b)) (cons a c)))
   (declaration #'(lambda (a) (cons a nil))))
  (dimension
   ([ identifier ] #'(lambda (a b c) (declare (ignore a c)) (cons b nil)))
   ([ number ] #'(lambda (a b c) (declare (ignore a c)) (cons b nil)))
   ([ number ] dimension (lambda (a b c d) (declare (ignore a c)) (cons b d)))
   ([ identifier ] dimension (lambda (a b c d) (declare (ignore a c)) (cons b d))))
  (declaration
   (identifier declaration #'cons)
   (identifier dimension)
   (identifier #'(lambda (a) (cons a nil))))))

(labels ((type-converter (type-spec)
		   (print type-spec)
		   (ecase (length type-spec)
			 (1 (case (car type-spec)
				  (short 's2)
				  (char 's1)
				  (boolean 's1)
				  (otherwise
				   (car type-spec))))
			 (2 (case (cadr type-spec)
				  (short 'u2)
				  (int 'u4)
				  (char 'u1)
				  (otherwise (cadr type-spec)))))))
  (defun find-type (declaration)
	(let ((declaration (reverse declaration)))
	  (if (consp (car declaration))
		  (let ((dimension (map 'list #'read-from-string (car declaration)))
				(name (cadr declaration))
				(type (type-converter (reverse (cddr declaration)))))
			`(,name (binary-vector ,type ',dimension)))
		  (let ((name (car declaration))
				(type (type-converter (reverse (cdr declaration)))))
			`(,name ,type))))))

#+cl-ppcre
(defmacro define-c-struct (name flags files-to-search))