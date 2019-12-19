;;;; day19.lisp

(in-package :aoc2019.day19)

(defvar *code* (first (read-puzzlefile 19)))

(defun position-affected-p (x y)
  (= 1
     (first (prog-outputs (execute-program! (load-program
					     *code*
					     :inputs (list x y)))))))

(defun day19 ()
  (loop
     :for x :from 0 :below 50
     :sum (loop
	     :for y :from 0 :below 50
	     :counting (position-affected-p x y))))

(defun find-affected-start (y &optional (start-x 0))
  (loop
     :for x :from start-x
     :until (position-affected-p x y)
     :finally (return x)))

(defun day19-part2 (&optional (size 100))
  (loop
     :with edge-length := (- size 1)
     :for y :from size
     :for x := (find-affected-start y) :then (find-affected-start y x)
     :until (position-affected-p (+ x edge-length) (- y edge-length))
     :finally (return (+ (- y edge-length) (* 10000 x)))))
