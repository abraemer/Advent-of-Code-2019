;;;; day02.lisp

(in-package #:aoc2019.day02)

(defun load-program (changed-values)
  (let* ((raw-data (extract-integers (first (read-puzzlefile 2))))
	 (program (make-array (length raw-data)
			      :element-type 'fixnum
			      :initial-contents raw-data)))
    (loop
       :for (place value) :in changed-values
       :do (setf (aref program place) value))
    program))

(defun execute-instruction! (program-data ip)
  (let* ((instr (aref program-data ip))
	 (a-addr (aref program-data (+ 1 ip)))
	 (b-addr (aref program-data (+ 2 ip)))
	 (a (aref program-data a-addr))
	 (b (aref program-data b-addr))
	 (target (aref program-data (+ 3 ip))))
    ;(format t "IP: ~a~t -> ~a ~a ~a ~a~%" ip instr a b target)
    (setf (aref program-data target)
	  (case instr
	    (1 (+ a b))
	    (2 (* a b))
	    (t (error (format nil "Unknown instruction ~a at ~a.~%" instr ip)))))))

(defun execute-program (param1 param2)
  (loop
     :with program-data := (load-program `((1 ,param1) (2 ,param2)))
     :for ip := 0 :then (incf ip 4)
     :until (equal 99 (aref program-data ip))
     :do (execute-instruction! program-data ip)
     :finally (return (aref program-data 0))))

(defun day2 (&optional (param1 12) (param2 2))
  (format t "The test program's output is ~a.~%" (execute-program param1 param2)))

(defun day2-2 (&optional (target 19690720))
  (loop :named outer
     :for noun :from 0 :upto 99
     :do (loop
	    :for verb :from 0 :up to 99
	    :for output := (execute-program noun verb)
	    :when (equal output target)
	    :do (format t "The solution is ~a." (+ verb (* 100 noun)))
	    :and :do (return-from outer (list noun verb)))))
