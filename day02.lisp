;;;; day02.lisp

(in-package #:aoc2019.day02)

(defun day2-helper (param1 param2)
  (evaluate-program! (load-program (first (read-puzzlefile 2))
				   :changes `((1 ,param1) (2 ,param2)))))

(defun day2 (&optional (param1 12) (param2 2))
  (format t "The test program's output is ~a.~%"
	  (day2-helper param1 param2)))

(defun day2-2 (&optional (target 19690720))
  (loop :named outer
     :for noun :from 0 :upto 99
     :do (loop
	    :for verb :from 0 :upto 99
	    :for output := (day2-helper noun verb)
	    :when (equal output target)
	    :do (format t "The solution is ~a." (+ verb (* 100 noun)))
	    :and :do (return-from outer (list noun verb)))))
