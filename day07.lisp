;;;; day07.lisp

(in-package :aoc2019.day07)

(defun permutations (list)
  (if (null list)
      (list nil)
      (loop
	 :for element :in list
	 :nconc (mapcar (lambda (subperm) (cons element subperm))
			(permutations (remove element list))))))

(defun try-settings-part1 (settings &optional (program-code (first (read-puzzlefile 7))))
  (loop
     :for phase :in settings
     :for next-input := 0 :then output
     :for output := (run-till-output! (load-program program-code :inputs (list phase next-input)))
     :finally (return output)))

(defun try-settings-part2 (settings &optional (program-code (first (read-puzzlefile 7))))
  (let ((thrusters (loop
		      :for phase :in settings
		      :collect (load-program program-code :inputs (list phase) :mem-factor 1))))
    (setf (cdr (last thrusters)) thrusters) ;make it a circular list
    (send-input (first thrusters) 0)
    (loop
       :for thruster :in thrusters
       :while (prog-running thruster)
       :for new-input := (run-till-output! thruster) :then (progn (send-input thruster new-input)
								  (run-till-output! thruster))
       :finally (return new-input))))

(defun day07 ()
  (let ((thruster-code (first (read-puzzlefile 7))))
    (format t "The maximal boost is ~a."
	    (loop
	       :for setting :in (permutations '(0 1 2 3 4))
	       :maximize (try-settings-part1 setting thruster-code)))))

(defun day07-part2 ()
  (let ((thruster-code (first (read-puzzlefile 7))))
    (format t "The highest signal possible is ~a."
	    (loop
	       :for setting :in (permutations '(5 6 7 8 9))
	       :maximize (try-settings-part2 setting thruster-code)))))
