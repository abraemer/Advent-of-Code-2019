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
    (aref mem ip)))

;;; opcode registry

(defparameter *instructions* (make-array 100))

(defun %register-instruction (opcode lambda)
  (setf (aref *instructions* opcode) lambda))

(defun %lookup-opcode (opcode)
  (unless (<= 0 opcode 99)
    (error "Opcode out of range! Got: ~a.~%" opcode))
  (let ((func (aref *instructions* opcode)))
    (when (null func)
      (error "No function registered for opcode ~a.~%" opcode))
    func))

(defmacro define-intcode (opcode (&rest parameters) &body body)
  (alexandria:with-gensyms (mem ip program continue-p)
    `(%register-instruction
      ,opcode
      (lambda (,program)
	(let ((,continue-p t)
	      (,mem (prog-memory ,program))
	      (,ip (prog-ip ,program)))
	  (declare (ignorable ,mem ,ip))
	  (macrolet ((reg (adress)
		       (list 'aref ',mem adress))
		     (halt ()
		       (list 'setf ',continue-p 'nil)))
	    (let ,(loop
		     :for param :in parameters
		     :for index :upfrom 1
		     :collect `(,param (reg (+ ,ip ,index))))
	      ,@body)
	    (values,(length parameters) ,continue-p)))))))

;;; Execution of a program

(defun set-parameters! (program params)
  (loop
     :with memory := (prog-memory program)
     :for (place value) :in params
     :do (setf (aref memory place) value))
  program)

(defun execute-instruction! (program)
  (multiple-value-bind (inc continue-p) (funcall (%lookup-opcode (get-next-opcode program))
						 program)
    (when continue-p
      (incf (prog-ip program) (+ 1 inc)))
    continue-p))

(defun execute-program! (program)
  (loop
     :while (execute-instruction! program)))

(defun evaluate-program! (program)
  (execute-program! program)
  (aref (prog-memory program) 0))

;;; Known opcodes

(define-intcode 1 (a b output)
  (setf (reg output) (+ (reg a) (reg b))))
(define-intcode 2 (a b output)
  (setf (reg output) (* (reg a) (reg b))))
(define-intcode 99 ()
  (halt))
