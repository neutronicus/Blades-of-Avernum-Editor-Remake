(in-package :boa-editor)

(define-binary-class scenario-big-endian ()
  ((scen-data scenario-data-type-big-endian)
   (outdoor-data (binary-vector-type :type 'outdoor-record-type-big-endian
								:dims (list (* (out-width scen-data) (out-height scen-data)))))
   (town-data (towns-vector :scenario-data scen-data))))

(define-binary-type towns-vector (scenario-data)
  (:reader (in)
		   (let* ((town-sizes (town-size scenario-data))
				  (to-return (make-array (num-towns scenario-data))))
			 (loop for i from 0 to (1- (length to-return)) doing
				  (setf (aref to-return i) (make-instance
											'town
											:record (read-value 'town-record-type-big-endian in)
											:layout (read-value (ecase (aref town-sizes i)
																  (0 'big-tr-type-big-endian)
																  (1 'ave-tr-type-big-endian)
																  (2 'tiny-tr-type-big-endian))
																in)))
				  finally (return to-return))))
  (:writer (out vec)
		   (let ((town-sizes (town-size scenario-data)))
			 (loop
				for town in vec
				for size in town-sizes doing
				(progn (write-value out
									'town-record-type-big-endian
									(record town))
					   (write-value out
									(ecase size
									  (0 'big-tr-type-big-endian)
									  (1 'ave-tr-type-big-endian)
									  (2 'tiny-tr-type-big-endian))
									(layout town)))))))

(defclass town ()
  ((record :initarg :record :accessor record)
   (layout :initarg :layout :accessor layout)))


(make-boa-defines "/Users/adamvh/Documents/Programming/Lisp/BOAEditorRemake/global.h")
(define-boa-datastructures (scenario-data-type town-record-type outdoor-record-type tiny-tr-type ave-tr-type big-tr-type))