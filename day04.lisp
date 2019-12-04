;;;; day04.lisp

(in-package :aoc2019.day04)

(defun is-valid (number)
  (loop
     :with doubles := nil
     :for last := nil :then value
     :for value :across (write-to-string number)
     :when (equal last value) :do (setf doubles t)
     :when (and last (char< value last)) :do (return nil)
     :finally (return doubles)))

(defun is-valid-part2 (number)
  (loop
     :with doubles := nil
     :with current-streak := 0
     :for last := nil :then value
     :for value :across (write-to-string number)
     :do (cond
	   ((and last (char< value last)) (return nil))
	   ((equal last value) (incf current-streak))
	   (t (when (= 2 current-streak) (setf doubles t))
	      (setf current-streak 1)))
     :finally (return (or doubles (= current-streak 2)))))

(defun day04 (&optional (start 246540) (end 787419))
  (loop
     :for current-value :from start :upto end
     :counting (is-valid current-value) :into part1
     :counting (is-valid-part2 current-value) :into part2
     :finally (return (values part1 part2))))
