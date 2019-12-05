;;;; day05.lisp

(in-package :aoc2019.day05)

(defun day05 ()
  ;;could automate the input but so far not done
  (execute-program! (load-program (first (read-puzzlefile 5)))))
