;;;; intcode.lisp
;;;; Implements a facility to execute Intcode programs

(in-package :aoc2019.intcode)

;;; program struct + helpers

(defstruct (intcode-program (:conc-name prog-))
  memory
  (inputs nil)
  (outputs nil)
  (running t)
  (ip 0 :type fixnum)
  (relative-base 0 :type fixnum))

(defun load-program (string &key (changes nil) (inputs nil) (outputs nil) (mem-factor 100))
  (let* ((raw-data (extract-integers string))
	 (program (make-array (* (length raw-data) mem-factor)
			      :element-type 'fixnum
			      :initial-element 0)))
    (loop
       :for value :in raw-data
       :for index :upfrom 0
       :do (setf (aref program index) value))
    (loop
       :for (place value) :in changes
       :do (setf (aref program place) value))
    (make-intcode-program :memory program :inputs inputs :outputs outputs)))

(defun copy-program (program)
  (let ((copy (copy-structure program)))
    (setf (prog-memory copy) (copy-list (prog-memory program)))
    (when (listp (prog-inputs program))
      (setf (prog-inputs copy) (copy-list (prog-inputs program))))
    (when (listp (prog-outputs program))
      (setf (prog-outputs copy) (copy-list (prog-outputs program))))
    copy))

(defun get-next-opcode (program)
  (let ((ip (prog-ip program))
	(mem (prog-memory program)))
    (unless (array-in-bounds-p mem ip)
      (error "Instruction pointer gone haywire! Instruction count:~a ~t IP: ~a~%" (length mem) ip))
    (mod (aref mem ip) 100)))

(defun get-next-parameter-modes (program length)
  (let ((ip (prog-ip program))
	(mem (prog-memory program)))
    (unless (array-in-bounds-p mem ip)
      (error "Instruction pointer gone haywire! Instruction count:~a ~t IP: ~a~%" (length mem) ip))
    (loop :repeat length
       :for (param-modes curr-mode) := (multiple-value-list (floor (floor (aref mem ip) 100) 10))
       :then (multiple-value-list (floor param-modes 10))
       :collect curr-mode)))

(defun send-input (program input)
  (setf (prog-inputs program) (append (prog-inputs program) (list input))))

;;; opcode registry

(defparameter *instructions* (make-array 100 :initial-element nil))

(defun %register-instruction (opcode lambda)
  (setf (aref *instructions* opcode) lambda))

(defun %lookup-opcode (opcode)
  (let ((func (aref *instructions* (mod opcode 100))))
    (when (null func)
      (error "No function registered for opcode ~a.~%" opcode))
    func))

(defmacro define-intcode (opcode (&rest parameters) &body body)
  (alexandria:with-gensyms (mem ip program mode-list jumped rel-base)
    `(%register-instruction
      ,opcode
      (lambda (,program)
	(let ((,jumped nil)
	      (,mem (prog-memory ,program))
	      (,ip (prog-ip ,program))
	      (,rel-base (prog-relative-base ,program))
	      (,mode-list (get-next-parameter-modes ,program ,(length parameters))))
	  (declare (ignorable ,mem ,ip ,mode-list))
	  ;; define methods for interacting with the program's execution
	  (macrolet ((reg (adress)
		       (list 'aref ',mem adress)))
	    (flet ((jump (to)
		     (setf ,jumped t)
		     (setf (prog-ip ,program) to))
		   (halt ()
		     (setf (prog-running ,program) nil))
		   (input ()
		     (let ((inputs (prog-inputs ,program)))
		       (cond
			   ((null inputs)
			    (format t "Input please: >") (read))
			 ((listp inputs) (pop (prog-inputs ,program)))
			 ((functionp inputs) (funcall inputs)))))
		   (output (something)
		     (if (functionp (prog-outputs ,program))
			 (funcall (prog-outputs ,program) something)
			 (push something (prog-outputs ,program))))
		   (adjust-relative-base (delta)
		     (incf (prog-relative-base ,program) delta)
		     (incf ,rel-base delta)))
	      (declare (ignorable #'jump #'halt #'input #'output #'adjust-relative-base))
	      ;; bind parameters to values
	      ;; respecting the parameter mode
	      (symbol-macrolet ,(loop
				   :for param :in parameters
				   :for index :upfrom 0
				   :for param-value-at := `(+ ,ip 1 ,index)
				   :collect `(,param (reg (ecase (nth ,index ,mode-list)
							    (0 (reg ,param-value-at))
							    (1 ,param-value-at)
							    (2 (+ (reg ,param-value-at) ,rel-base))))))
		,@body)))
	  (when (and (prog-running ,program) (not ,jumped))
	    (incf (prog-ip ,program) (+ 1 ,(length parameters))))
	  (prog-running ,program))))))

;;; Execution of a program

(defun set-parameters! (program params)
  (loop
     :with memory := (prog-memory program)
     :for (place value) :in params
     :do (setf (aref memory place) value))
  program)

(defun execute-instruction! (program)
  (funcall (%lookup-opcode (get-next-opcode program))
	   program))

(defun execute-program! (program)
  (loop :while (execute-instruction! program))
  program)

(defun run-till-output! (program)
  (loop
     :with initial-length := (length (prog-outputs program))
     :while (execute-instruction! program)
     :until (> (length (prog-outputs program)) initial-length))
  (first (prog-outputs program)))

;;; Known opcodes

(define-intcode 1 (a b output)
  (setf output (+ a b)))
(define-intcode 2 (a b output)
  (setf output (* a b)))
(define-intcode 3 (a)
  (setf a (input)))
(define-intcode 4 (a)
  (output a))
(define-intcode 5 (bool addr) ;jump-if-true
  (unless (= bool 0)
    (jump addr)))
(define-intcode 6 (bool addr) ;jump-if-false
  (when (= bool 0)
    (jump addr)))
(define-intcode 7 (a b output) ;less-than
  (setf output (if (< a b) 1 0)))
(define-intcode 8 (a b output)
  (setf output (if (= a b) 1 0))) ;equals
(define-intcode 9 (delta) ;adjust relative base
  (adjust-relative-base delta))
(define-intcode 99 ()
  (halt))
