;;;; day12.lisp

(in-package :aoc2019.day12)

(defun load-planets ()
  (loop-line-by-line (puzzlefile 12)
     :collect (list (extract-integers line) (list 0 0 0))))

(defun coord-selector (x)
  (lambda (planet)
    (list (list (nth x (first planet)))
	  (list (nth x (second planet))))))

(defun apply-gravity! (planet1 planet2)
  (loop
     :for pos1 :in (first planet1)
     :for pos2 :in (first planet2)
     :for index :upfrom 0
     :for delta := (cond
		     ((> pos1 pos2) -1)
		     ((< pos1 pos2)  1)
		     (t 0))
     :do (incf (nth index (second planet1)) delta)
       (decf (nth index (second planet2)) delta)))

(defun simulate! (planets)
  (loop
     :for sublist :on planets
     :for planet1 := (first sublist)
     :do (loop
	    :for planet2 :in (rest sublist)
	    :do (apply-gravity! planet1 planet2))
       (setf (first planet1) (mapcar #'+ (first planet1) (second planet1))))
  planets)

(defun total-energy (planets)
  (loop
     :for planet :in planets
     :sum (* (reduce #'+ (mapcar #'abs (first planet)))
	     (reduce #'+ (mapcar #'abs (second planet))))))

(defun day12 ()
  (loop :with planets := (load-planets)
     :repeat 1000
     :do (simulate! planets)
     :finally (return (total-energy planets))))

;planets here has the structure ((coord vel) (coord vel)... )

(defun copy-planets (planets)
  (mapcar (lambda (l) (mapcar #'copy-list l)) planets))

(defun detect-loop (planets)
  (loop
     :for current := (simulate! (copy-planets planets)) :then (simulate! current)
     :for step-count :upfrom 1
     :until (equalp current planets)
     :finally (return step-count)))

(defun day12-2 ()
  (let ((planets (load-planets)))
    (apply #'lcm (mapcar (lambda (coord) (detect-loop (mapcar (coord-selector coord) planets)))
			 '(0 1 2)))))
