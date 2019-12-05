;;;; intcode.lisp
;;;; Implements a facility to execute Intcode programs

(in-package :aoc2019.intcode)

;;; program struct + helpers

(defstruct (intcode-program (:conc-name prog-))
  memory
  (ip 0 :type fixnum))

(defun load-program (string &optional (changes nil))
  (let* ((raw-data (extract-integers string))
	 (program (make-array (length raw-data)
			      :element-type 'fixnum
			      :initial-contents raw-data)))
    (loop
       :for (place value) :in changes
       :do (setf (aref program place) value))
    (make-intcode-program :memory program)))

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

;;; opcode registry

(defparameter *instructions* (make-array 100))

(defun %register-instruction (opcode lambda)
  (setf (aref *instructions* opcode) lambda))

(defun %lookup-opcode (opcode)
  (let ((func (aref *instructions* (mod opcode 100))))
    (when (null func)
      (error "No function registered for opcode ~a.~%" opcode))
    func))

(defmacro define-intcode (opcode (&rest parameters) &body body)
  (alexandria:with-gensyms (mem ip program continue-p mode-list jumped)
    `(%register-instruction
      ,opcode
      (lambda (,program)
	(let ((,continue-p t)
	      (,jumped nil)
	      (,mem (prog-memory ,program))
	      (,ip (prog-ip ,program))
	      (,mode-list (get-next-parameter-modes ,program ,(length parameters))))
	  (declare (ignorable ,mem ,ip ,mode-list))
	  (macrolet ((reg (adress)
                       (list 'aref ',mem adress)))
	    (flet ((jump (to)
		     (setf ,jumped t)
		     (setf (prog-ip ,program) to))
		   (halt ()
		     (setf ,continue-p 'nil))
					;define this to abstract the input - maybe want to automatize at some point
		   (input ()
		     (read)))
	      (declare (ignorable #'jump #'halt #'input))
	      (symbol-macrolet ,(loop
				   :for param :in parameters
				   :for index :upfrom 0
				   :for param-value-at := `(+ ,ip 1 ,index)
				   :collect `(,param (reg (ecase (nth ,index ,mode-list)
							    (0 (reg ,param-value-at))
							    (1 ,param-value-at)))))
		,@body)))
	  (unless ,jumped
	    (incf (prog-ip ,program) (+ 1 ,(length parameters))))
	  ,continue-p)))))

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
  (loop
     :while (execute-instruction! program)))

(defun evaluate-program! (program)
  (execute-program! program)
  (aref (prog-memory program) 0))

;;; Known opcodes

(define-intcode 1 (a b output)
  (setf output (+ a b)))
(define-intcode 2 (a b output)
  (setf output (* a b)))
(define-intcode 99 ()
  (halt))
(define-intcode 3 (a)
  (format t "~%Input pls: >")
  (setf a (input)))
(define-intcode 4 (a)
  (format t "~a~%" a))
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
