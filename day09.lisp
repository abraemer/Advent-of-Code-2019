;;;; day09.lisp

(in-package :aoc2019.day09)

(defun day09 ()
  (values
   (first (prog-outputs (execute-program! (load-program (first (read-puzzlefile 9)) :inputs '(1)))))
   (first (prog-outputs (execute-program! (load-program (first (read-puzzlefile 9)) :inputs '(2)))))))
