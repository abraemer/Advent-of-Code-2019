;;;; day10.lisp

(in-package :aoc2019.day10)

(defun load-asteroids ()
  (loop-line-by-line (puzzlefile 10)
     :for y :from 0
     :nconc (loop
	       :for char :across line
	       :for x :from 0
	       :when (char= char #\#) :collect (complex x y))))

(defun line-of-sight (a b)
  (let* ((delta (- b a)))
    (/ delta (gcd (realpart delta) (imagpart delta)))))

(defun build-all-lines-of-sight (asteroids)
  (loop
     :with visibles := (make-hash-table :test 'equal)
     :for current-sublist := asteroids :then (rest current-sublist)
     :while current-sublist
     :do (loop
	    :with current := (first current-sublist)
	    :for other :in (rest current-sublist)
	    :for los := (line-of-sight current other)
	    :do (push los (gethash current visibles))
	    :do (push (- los) (gethash other visibles)))
     :finally (return visibles)))

(defun vec->angle (complex)
  (let ((value
	 (- (/ pi 2) (atan (- (imagpart complex)) (realpart complex)))))
    (if (> 0 (round value 1e-5))
	(+ value (* 2 pi))
	value)))

(defun find-station-location (lines-of-sight)
  (loop
     :with station-location := -1
     :for location :being :the :hash-key
     :using (hash-value visible) :of lines-of-sight
     :for visible-count := (length (remove-duplicates visible))
     :maximizing visible-count :into max-count
     :when (= max-count visible-count) :do (setf station-location location)
     :finally (return (values station-location max-count))))

(defun day10 ()
  (let ((lines-of-sight (build-all-lines-of-sight (load-asteroids))))
    (multiple-value-bind (station-location visible-count) (find-station-location lines-of-sight)
      (format t "The optimal spot for the station is (~a|~a) and there are ~a asteroids visible.~%"
	      (realpart station-location) (imagpart station-location) visible-count)
      (loop :with direction := (nth 199
				(sort (remove-duplicates
				       (gethash station-location lines-of-sight))
				      #'< :key #'vec->angle))
	 :for curr-pos := (+ station-location direction) :then (+ curr-pos direction)
	 :until (gethash curr-pos lines-of-sight nil)
	 :finally (format t "The 200th asteroid to be obliterated is at (~a|~a).~%"
			  (realpart curr-pos) (imagpart curr-pos))))))
