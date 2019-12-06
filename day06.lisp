;;;; day06.lisp

(in-package :aoc2019.day06)

(defun parse-input (&optional (file (puzzlefile 6)))
  (loop-line-by-line file
     :with map := (make-hash-table)
     :for (center orbitee) := (mapcar 'read-from-string (split-seq line #\)))
     :do (setf (gethash orbitee map) center)
     :finally (return map)))

(defun orbit-count (map object)
  (loop
     :for current := (gethash object map nil) :then (gethash current map nil)
     :while current
     :count t))

(defun build-hierarchy (map start)
  (loop
     :for current := (gethash start map nil) :then (gethash current map nil)
     :while current
     :collect current))

(defun day06 ()
  (let ((map (parse-input)))
    (format t "There are ~a total orbits in the map.~%It takes ~a transfers to get to santa."
	    (loop
	       :for object :being :the :hash-keys :of map
	       :sum (orbit-count map object))
	    (length (nset-exclusive-or (build-hierarchy map 'san) (build-hierarchy map 'you))))))
