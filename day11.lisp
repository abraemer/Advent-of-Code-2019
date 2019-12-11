;;;; day11.lisp

(in-package :aoc2019.day11)

(defun make-circular! (list)
  (setf (cdr (last list)) list))

(defun day11 ()
  ;; ugly but straight-forward implementation
  (loop
     :with program := (load-program (first (read-puzzlefile 11)))
     :with tiles := (make-hash-table :test 'equal)
     :with position := 0
     :with direction := #C(0 1)
     :while (prog-running program)
     :do
       (setf (prog-inputs program) (make-circular! (list (gethash position tiles 0))))
       (setf (gethash position tiles) (run-till-output! program))
       (setf direction (* direction (ecase (run-till-output! program)
				      (0 #C(0  1))
				      (1 #C(0 -1)))))
       (incf position direction)
     :finally (return (hash-table-count tiles))))

(defun day11-part2 ()
  (let ((tiles (make-hash-table :test 'equal)))
    (setf (gethash 0 tiles) 1)
    (loop
       :with program := (load-program (first (read-puzzlefile 11)))
       :with position := 0
       :with direction := #C(0 1)
       :while (prog-running program)
       :do
	 (setf (prog-inputs program) (make-circular! (list (gethash position tiles 0))))
	 (setf (gethash position tiles) (run-till-output! program))
	 (setf direction (* direction (ecase (run-till-output! program)
					(0 #C(0  1))
					(1 #C(0 -1)))))
	 (incf position direction)
       :finally (return (hash-table-count tiles)))
    (let* ((low (apply #'min (mapcar #'imagpart (hash-keys tiles))))
	   (high (apply #'max (mapcar #'imagpart (hash-keys tiles))))
	   (left (apply #'min (mapcar #'realpart (hash-keys tiles))))
	   (right (apply #'max (mapcar #'realpart (hash-keys tiles)))))
      (format t "~{~{~a~}~%~}" (loop
		      :for y :from high :downto low
		      :collect (loop :for x :from left :upto right
				  :collect (ecase (gethash (complex x y) tiles 0)
					     (0 #\SPACE)
					     (1 #\X))))))))
