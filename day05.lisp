;;;; day05.lisp

(in-package :aoc2019.day05)

(defun day05 ()
  (first (prog-outputs (execute-program! (load-program (first (read-puzzlefile 5))
						       :inputs '(1))))))

(defun day05-2 ()
  (first (prog-outputs (execute-program! (load-program (first (read-puzzlefile 5))
						       :inputs '(5))))))
