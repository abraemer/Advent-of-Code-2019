;;;; day16.lisp

(in-package :aoc2019.day16)

(defun char->int (char)
  (- (char-int char) #.(char-int #\0)))

(defun int->char (int)
  (code-char (+ #. (char-int #\0) int)))

(defun load-data ()
  (map 'vector #'char->int (first (read-puzzlefile 16))))

(defun pattern-factor (cycle-length position)
  (alexandria:switch ((mod (floor (1+ position) cycle-length) 4) :test #'=)
    (0  0)
    (1  1)
    (2  0)
    (3 -1)))

(defun fft (input &optional (rounds 1))
  (loop
     :with input := (alexandria:copy-array input)
     :with output := (make-array (array-total-size input) :element-type '(mod 10))
     :repeat rounds
     :do 
       (loop
	  :for index :from 0 :below (array-total-size input)
	  :do (setf (aref output index) (mod (abs (loop
						     :for val :across input
						     :for position :from 0
						     :sum (* (pattern-factor (1+ index) position) val)))
					     10)))
       (rotatef input output)
     :finally (return input)))

(defun day16 ()
  (let ((result
	 (fft (map 'vector #'char->int (first (read-puzzlefile 16))) 100)))
    (coerce (loop :for index :below 8
	       :collect (int->char (aref result index)))
	    'string)))
