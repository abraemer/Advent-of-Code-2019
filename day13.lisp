;;;; day13.lisp

(in-package :aoc2019.day13)

(defun count-blocks (board)
  (loop :for head := board :then (rest (rest (rest head)))
     :while head :count (= (first head) 2)))

(defun day13 ()
  (count-blocks (prog-outputs (execute-program! (load-program (first (read-puzzlefile 13)))))))

(defun show-board (board)
  (loop for i below (first (array-dimensions board)) do
        (loop :for j :below (second (array-dimensions board)) :do
          (format t "~a " (aref board i j)))
        (format t "~%")))

(defun day13-part2 ()
  (let (;(board (make-array '(24 40)))
	(score nil)
	(ball-xpos nil)
	(paddle-xpos nil)
	(program (load-program (first (read-puzzlefile 13)) :changes '((0 2)))))
    (labels ((parse-prog-output! ()
	       (loop
		  :with input := (nreverse (prog-outputs program))
		  :while input
		  :for x := (pop input)
		  :for y := (pop input)
		  :for tile := (pop input)
		  :do (case tile
			( 3 (setf paddle-xpos x))
			( 4 (setf ball-xpos x)))
		  :if (= -1 x) :do (setf score tile)
		  ;:else :do (setf (aref board y x) tile)
		    )
	       (setf (prog-outputs program) nil))
	     (paddle-move ()
	       (parse-prog-output!)
					;(format t "Block count: ~a~tScore: ~a~%" (loop :for index :from 0 :below (array-total-size board) :count (= 2 (row-major-aref board index))) score)
					;(show-board board)
					;(format t "Input: ")
	       (cond
		 ((> ball-xpos paddle-xpos)  1)
		 ((< ball-xpos paddle-xpos) -1)
		 (t 0))))
      (setf (prog-inputs program) #'paddle-move)
      (execute-program! program)
      (parse-prog-output!)
      score)))
