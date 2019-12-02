;;;; day01.lisp

(in-package :aoc2019.day01)

(defun mass->fuel (mass)
  (- (floor mass 3) 2))

(defun additional-fuel (fuel)
  ;; Possible optimization: make this tail recursive
  (if (<= fuel 8)
      0
      (let ((fuel-for-fuel (mass->fuel fuel)))
	(+ fuel-for-fuel (additional-fuel fuel-for-fuel)))))

(defun day01 ()
  (loop-line-by-line (puzzlefile 1)
     :for mass := (parse-integer line)
     :for fuel := (mass->fuel mass)
     :sum fuel :into total-fuel
     :sum (additional-fuel fuel) :into total-additional-fuel
     :finally (format t "The total fuel for the modules is ~a.~%The total fuel (inclusive fuel for fuel) needed is ~a." total-fuel (+ total-fuel total-additional-fuel))))
