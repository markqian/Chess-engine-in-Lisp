(load "~/chess/src/eval.lisp")

;;classic alpha beta search function
(defun alphaBeta (b alpha beta depthleft)
  (let* ((isCheck (isChecked b))
	 (moves (if isCheck
		    (genCheckEvasions b)
		  (generate-all-moves b)))
	 (m '())
	 (bestscore -1000))
    (if (= depthleft 0) (simple-eval b)      
      (if (loop for move in moves do
		(applyMove b move)
		(if (opIsChecked b)
		    (popMove b)
		  (let* ((score (- (alphaBeta b (- beta) (- alpha)  (- depthleft 1)))))

		    (popMove b)
		    (if (>= score beta) (return t))
		    (if (> score bestscore) 
			(progn
			  (setq bestscore score)
			  (if (> score alpha)
			      (setq alpha score))
			  (setq m move))))))
	  (values beta m)
	(values alpha m)))))

;;wrapper for getting score and move values
(defun getResult (b alpha beta depth)
  (multiple-value-bind (score move) (alphaBeta b alpha beta depth)(list score move)))
