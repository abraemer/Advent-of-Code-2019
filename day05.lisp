;;;; day05.lisp

(in-package :aoc2019.day05)

(defun day05 ()
  (last (execute-program! (load-program (first (read-puzzlefile 5))
					:inputs '(1)))))

(defun day05-2 ()
  (last (execute-program! (load-program (first (read-puzzlefile 5))
					:inputs '(5)))))
