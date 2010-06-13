(in-package :boa-editor)

;; These two forms define, respectively, the (somewhat crude) lexer and parser that I use to crawl
;; the BoA editor source files for data type definitions.  This way, once I know which types of
;; data the BoA editor writes to disk, I don't have to copy their definitions (nor the defintions
;; they depend on) by hand.
(defmacro define-c-struct-lexer (name)
  `(define-string-lexer ,name
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

(define-c-struct-lexer g!lexer)

(define-parser c-struct-parser
  (:start-symbol record)
  (:terminals ({ } |(| |)| [ ] |;| |,| |:| identifier number class public typedef struct class))
  (record
   (typedef struct { declaration-list } identifier |;|
			#'(lambda (a b c d e f g)
				(declare (ignore a b c e g))
				(list f (map 'list #'identity d))))
   (class identifier { public |:| declaration-list } |;|
		  #'(lambda (a b c d e f g h)
			  (declare (ignore a c d e g h))
			  (list b (map 'list #'identity f)))))
  (declaration-list
   (declaration |;| declaration-list #'(lambda (a b c) (declare (ignore b)) (cons a c)))
   (declaration |,| comma-declaration-list |;|
				#'(lambda (a b c d)
					(declare (ignore b d))
					(cons a
						  (concatenate 'list
									   (map 'list
											#'(lambda (x)
												(concatenate 'list
															 (subseq a
																	 0
																	 (if (consp (car (last a)))
																		 (- (length a) 2)
																		 (1- (length a)))) x)) c)
									   nil))))
   (declaration |,| comma-declaration-list |;| declaration-list
				#'(lambda (a b c d e)
					(declare (ignore b d))
					(cons a
						  (concatenate 'list
									   (map 'list
											#'(lambda (x)
												(concatenate 'list
															 (subseq a
																	 0
																	 (if (consp (car (last a)))
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
   ([ identifier ] #'(lambda (a b c) (declare (ignore a c)) (cons (add-earmuffs b) nil)))
   ([ number ] #'(lambda (a b c) (declare (ignore a c)) (cons b nil)))
   ([ number ] dimension (lambda (a b c d) (declare (ignore a c)) (cons b d)))
   ([ identifier ] dimension (lambda (a b c d) (declare (ignore a c)) (cons (add-earmuffs b) d))))
  (declaration
   (identifier declaration #'cons)
   (identifier dimension)
   (identifier #'(lambda (a) (cons a nil)))))

