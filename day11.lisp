;;;; day11.lisp

(in-package :aoc2019.day11)

(defun day11 (&optional (part2 nil))
  (let ((position 0)
	(direction #C(0 1))
	(tiles (make-hash-table :test 'equal)))
    (when part2
      (setf (gethash 0 tiles) 1))
    (flet ((look ()
	     (gethash position tiles 0))
	   (paint (color)
	     (setf (gethash position tiles) color))
	   (move (output)
	     (setf direction (* direction (ecase output
					    (0 #C(0 1))
					    (1 #C(0 -1)))))
	     (incf position direction)))
      (let ((next-output (make-circular! (list #'paint #'move))))
	(execute-program! (load-program (first (read-puzzlefile 11))
					:inputs #'look
					:outputs (lambda (o) (funcall (pop next-output) o))))))
    (when part2 ;;output the tiles
      (let* ((low (apply #'min (mapcar #'imagpart (hash-keys tiles))))
	     (high (apply #'max (mapcar #'imagpart (hash-keys tiles))))
	     (left (apply #'min (mapcar #'realpart (hash-keys tiles))))
	     (right (apply #'max (mapcar #'realpart (hash-keys tiles)))))
	(format t "~{~{~a~}~%~}" (loop
				    :for y :from high :downto low
				    :collect (loop :for x :from left :upto right
						:collect (ecase (gethash (complex x y) tiles 0)
							   (0 #\SPACE)
							   (1 #\X)))))))
    (hash-table-count tiles)))

(defun day11-part2 ()
  (day11 t))
