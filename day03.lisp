;;;; day03.lisp

(in-package :aoc2019.day03)

;;; Helpers

(defun manhattan-dist (vec)
  (+ (abs (realpart vec))
     (abs (imagpart vec))))

(defun direction->vector (direction)
  (case direction
    (u (complex  0  1))
    (d (complex  0 -1))
    (l (complex  1  0))
    (r (complex -1  0))))

;;; Wire iteration facility

(defun parse-wire-segment (segment)
  (values (direction->vector (read-from-string segment nil nil :end 1)) ;direction vec
	  (read-from-string segment nil nil :start 1))) ;distance

(defun wire-segment-iterator (start wire-segment)
  (multiple-value-bind (vec dist) (parse-wire-segment wire-segment)
    (let ((i 0));excludes start-point
      (lambda ()
	(unless (>= i dist)
	  (incf i)
	  (+ start (* i vec)))))))

(defun wire-iterator (wire-raw &optional (position 0))
  (let* ((segment-list (split-seq wire-raw #\,))
	 (curr-segment-it nil))
    (labels ((rec ()
	       (if (and (null curr-segment-it))
		   (next-segment) ;start iteration
		   (let ((newpos (funcall curr-segment-it)))
		     (if (null newpos)
			 (next-segment) ;current iterator has run out
			 (setf position newpos)))))
	     (next-segment ()
	       (unless (null segment-list)
		 (setf curr-segment-it (wire-segment-iterator position (first segment-list))
		       segment-list (rest segment-list))
		 (rec))))
      #'rec)))

;;; Solution

(defun day03 ()
  (let ((wires (read-puzzlefile 3))
	(position-set (make-hash-table :test 'equal)))
    (loop
       :with wire-iterator := (wire-iterator (first wires))
       :for position := (funcall wire-iterator)
       :for stepcount :upfrom 1
       :while position
       :unless (gethash position position-set)
       :do (setf (gethash position position-set) stepcount))
    (loop
       :with wire-iterator := (wire-iterator (second wires))
       :for position := (funcall wire-iterator)
       :for stepcount :upfrom 1
       :while position
       :when (gethash position position-set)
       :minimize (manhattan-dist position) :into closest-intersection ; part 1
       :and :minimize (+ stepcount (gethash position position-set)) :into shortest-path ; part 2
       :finally (format t "The closest intersetion is ~a away.~%The smallest loop takes ~a steps.~%"
			closest-intersection shortest-path))))
