;;;; day08.lisp

(in-package :aoc2019.day08)

(defun parse-input (width height)
  (loop
     :with layersize := (* width height)
     :with input := (first (read-puzzlefile 8))
     :for index :from 0 :below (length input) :by layersize
     :collect (subseq input index (+ index layersize))))

(defun day08 (&optional (width 25) (height 6))
  (loop
     :with min-layer := nil
     :for layer :in (parse-input width height)
     :for zeros := (count #\0 layer)
     :when (< zeros min-zeros) :do (setf min-layer layer)
     :minimize zeros :into min-zeros
     :finally (return (* (count #\1 min-layer) (count #\2 min-layer)))))

(defun day08-part2 (&optional (width 25) (height 6))
  (format t "~{~{~a~}~^~%~}"
	  (loop ;row
	     :with data := (parse-input width height)
	     :for row :below height
	     :collect (loop ;column
			 :for index :from 0 :below width
			 :collect (loop ;layer
				     :for layer :in data
				     :for char := (aref layer (+ (* width row) index))
				     :while (char= char #\2)
				     :finally (return (ecase char
							(#\1 #\X)
							(#\0 #\SPACE))))))))
