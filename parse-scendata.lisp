(in-package :boa-editor)

(defmacro load-scen-data (filename)
  (with-open-file (f filename :direction :input)
				  (parse-with-lexer (scen-data-lexer (slurp-stream f))
									scenario-data-parser)))

(define-string-lexer scen-data-lexer
  ("^//.*[\\n\\r]+")
  ("//.*[\\n\\r]+")
  (";" (return (values '|;| '|;|)))
  ("=" (return (values '|=| '|=|)))
  ("beginscendatascript" (return (values 'beginscendatascript 'beginscendatascript)))
  ("clear" (return (values 'clear 'clear)))
  ("import" (return (values 'import 'import)))
  ("begindefine(\\w+)" (return (values 'definition-opener
									   (if (equal $1 "floor")
										   (intern (string-upcase "floor-type"))
										   (intern (string-upcase $1))))))
  ("\"(.+?)\"" (return (values 'str $1)))
  ("[A-Za-z]{2}_(\\w+)" (return (values 'identifier (intern (string-upcase (regex-replace-all "_" $1 "-"))))))
  ("([-\\d]+)" (return (values 'number  $1))))

;; This parser translates a scenario data file into Lisp code - basically all the work is
;; done here.  Note that it assumes the existence of classes named creature, floor-type,
;; terrain, and item, whose slots are named identically to the corresponding fields in the
;; scenario data file, with the prefixes (i.e. cr_, fl_) removed and all underscores (_)
;; replaced with dashes (-)
;; These things will eventually be defined elsewhere in this project
(define-parser scenario-data-parser
  (:start-symbol scenario-data)
  (:terminals (|;| definition-opener str identifier number beginscendatascript clear |=| import))
  (scenario-data
   (beginscendatascript |;| definition-list
						#'(lambda (a b c)
							(declare (ignore a b))
							(cons 'progn c))))
  (definition-list
	  (definition definition-list #'cons)
	  (definition))
  (definition
	  (definition-opener number |;| clear |;| declaration-list
						 #'(lambda (a b c d e f)
							 (declare (ignorable c d e))
							 (make-scendata-assignments a b f `(make-instance ',a))))
	  (definition-opener number |;| clear |;|
						 #'(lambda (a b c d e)
							 (declare (ignorable c d e))
							 (make-scendata-assignments a b nil `(make-instance ',a))))
	  (definition-opener number |;| import |=| number |;| declaration-list
					   #'(lambda (a b c d e f g h)
						   (declare (ignorable c d e g))
						   (make-scendata-assignments a b h `(copy-instance
															  (aref ,(definition-type a) ,(read-from-string f))))))
	(definition-opener number |;| import |=| number |;|
					   #'(lambda (a b c d e f g)
						   (declare (ignorable c d e g))
						   (make-scendata-assignments a b nil `(copy-instance
															  (aref ,(definition-type a) ,(read-from-string f))))))
	(definition-opener number |;| declaration-list
						 #'(lambda (a b c d)
							 (declare (ignorable c))
							 (if (= (read-from-string b) 0)
								 (make-scendata-assignments a b d `(make-instance ',a))
								 (make-scendata-assignments a b d `(copy-instance *last-edited*)))))
	(definition-opener number |;|
	  #'(lambda (a b c)
		  (declare (ignorable c))
		  (make-scendata-assignments a b nil `(copy-instance *last-edited*)))))
  (declaration-list
   (declaration |;| declaration-list #'(lambda (a b c) (declare (ignore b)) (cons a c)))
   (declaration |;| #'(lambda (a b) (declare (ignore b)) (cons a nil))))
  (declaration
   (identifier |=| str
			   #'(lambda (a b c)
				   (declare (ignore b))
				   `(,a ,c)))
   (identifier |=| number
			   #'(lambda (a b c)
				   (declare (ignore b))
				   `(,a ,(read-from-string c))))
   (identifier number |=| number
			   #'(lambda (a b c d)
				   (declare (ignore c))
				   `(,a ,(read-from-string b) ,(read-from-string d))))))

(defun definition-type (opener)
  (ecase opener
	(creature '*creatures*)
	(floor-type '*floors*)
	(terrain '*terrains*)
	(item '*items*)))

(defun make-scendata-assignments (opener id assignments init-form)
  (macrolet ((array-element ()
			   ``(aref
				  ,(definition-type opener)
				  ,(read-from-string id))))
	`(progn
	   (setf
		,(array-element)
		,init-form)
	   ,@(map 'list
			  #'(lambda (x)
				  (ecase (length x)
					(2 `(setf
						 (,(car x)
						   ,(array-element))
						 ,(cadr x)))
					(3 `(setf
						 (aref (,(car x)
								 ,(array-element))
							   ,(cadr x))
						 ,(caddr x)))))
			  assignments)
	   (setf *last-edited* ,(array-element)))))

