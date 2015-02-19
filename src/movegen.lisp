(load "./src/attack.lisp")

;; generate new move based on move code
(defun newMove (fromcord tocord &optional (flag NORMAL_MOVE)) 
  (+ (aref shiftedFlags flag) (aref shiftedFromCords fromcord) tocord))

;; generate knight moves
(defun gen-knight-moves (b mask)
  (let* ((knights (aref (boards b) (color b) KNIGHT)))
    (do-bits (cord knights)
	     (flat) (collect 
		     (let ((knightMoves (aref moveArray KNIGHT cord)))
		       (do-bits (c (logand knightMoves mask))
				(collect (newMove cord c))))))))

;; generate king moves
(defun gen-king-moves (b mask)
  (let* ((cord (firstBit (aref (boards b) (color b) king)))
	 (kingMoves (aref moveArray KING cord)))
    (do-bits (c (logand kingMoves mask))
	     (collect (newMove cord c)))))

;; or of two attack bitboards
(defun or-attack (at1 at2) 
  (cond ((and at1 at2) (logior at1 at2))
	 ((or at1 at2) (or at1 at2))
	 (t 0)))


;; generate rook and queen moves
(defun gen-rooque-moves (b mask)
  (let* ((rooks (aref (boards b) (color b) ROOK))
	 (queens (aref (boards b) (color b) QUEEN))
	 (blockers (blocker b)))
    (do-bits (cord (logior rooks queens))
	     (flat) (collect
		     (let* ((val (logand (aref ray00 cord) blockers))
			    (val2 (logand (aref ray90 cord) blockers))
			    (attackBoard 
			     (or-attack (getAttack attack00 cord val)
				      (getAttack attack90 cord val2))))
		       (do-bits (c (logand attackBoard mask))
				(collect (newMove cord c))))))))

;; generate bishop and queen moves
(defun gen-bisque-moves (b mask)
  (let* ((bishops (aref (boards b) (color b) BISHOP))
	 (queens (aref (boards b) (color b) QUEEN))
	 (blockers (blocker b)))
    (do-bits (cord (logior bishops queens))
	     (flat) (collect (let* ((val (logand (aref ray45 cord) blockers))
				    (val2 (logand (aref ray135 cord) blockers))
				    (attackBoard 
					 (or-attack (getAttack attack45 cord val)
						    (getAttack attack135 cord val2))))
			       (do-bits (c (logand attackBoard mask))
					(collect (newMove cord c))))))))


;;generate one step white pawn moves
(defun onestep-whitepawn-moves (b)
  (if (= (color b) white)
      (let* ((pawns (aref (boards b) white PAWN))	
	     (notblocker (lognot (blocker b)))
	     (movedpawns (logand (ash pawns -8) notblocker)))
	(apply 'append 
	       (do-bits (cord movedpawns) 
			(collect (if (>= cord 56) 
				     (loop for p being the elements of PROMOTIONS
				       collect (newMove (- cord 8) cord p))
				   (list (newMove (- cord 8) cord)))))))))

;; generate two step white pawn moves
(defun twostep-whitepawn-moves (b)
  (if (= (color b) white)
      (let* ((pawns (aref (boards b) white PAWN))
	     (secondrow (logand pawns (aref rankBits 1)))
	     (notblocker (lognot (blocker b)))
	     (movedpawns (logand (ash secondrow -8) notblocker)))
	(setf movedpawns (logand (ash movedpawns -8) notblocker))
	(do-bits (cord movedpawns)
		 (collect (newMove (- cord 16) cord))))))
  
;; generate capture left white pawn moves
(defun captureleft-whitepawn-moves (b)
  (if (= (color b) white)
      (let* ((enpassants (enpassant b))
	     (enemies (getEnemies b white))
	     (pawns (aref (boards b) white PAWN))
	     (capLeftPawns (logand pawns (lognot (aref fileBits 0))))
	     (pawnEnemies (logior enemies (or (and (not (null enpassants)) 
						   (aref bitPosArray enpassants)) 0))))
    (setf capLeftPawns (logand (ash capLeftPawns -7) pawnEnemies))
    (apply 'append 
	   (do-bits (cord capLeftPawns)
		    (collect 
		     (cond ((>= cord 56) (loop for p being the elements of PROMOTIONS
					       collect (newMove (- cord 7) cord p)))
			   ((equal cord enpassants) (list (newMove (- cord 7) cord ENPASSANT)))
			   (t (list (newMove (- cord 7) cord))))))))))

;; generate capture right white pawn moves
(defun captureright-whitepawn-moves (b)
  (if (= (color b) white)
      (let* ((enpassants (enpassant b))
	     (enemies (getEnemies b white))
	     (pawns (aref (boards b) white PAWN))
	     (capLeftPawns (logand pawns (lognot (aref fileBits 0))))
	     (pawnEnemies (logior enemies (or (and (not (null enpassants)) 
						   (aref bitPosArray enpassants)) 0))))
	
	(setf capLeftPawns (logand (ash capLeftPawns -9) pawnEnemies))
	(apply 'append 
	       (do-bits (cord capLeftPawns)
			(collect 
			 (cond ((>= cord 56) (loop for p being the elements of PROMOTIONS
						   collect (newMove (- cord 9) cord p)))
			       ((equal cord enpassants) (list (newMove (- cord 9) cord ENPASSANT)))
			       (t (list (newMove (- cord 9) cord))))))))))
  
;; generate one step black pawn moves
(defun onestep-blackpawn-moves (b)
  (if (= (color b) black)
      (let* ((pawns (aref (boards b) black PAWN))	
	     (notblocker (lognot (blocker b)))
	     (movedpawns (logand (ash pawns 8) notblocker)))
	(apply 'append 
	       (do-bits (cord movedpawns) 
			(collect (if (<= cord 7) 
				     (loop for p being the elements of PROMOTIONS
					   collect (newMove (+ cord 8) cord p))
				   (list (newMove (+ cord 8) cord)))))))))

;; generate two step black pawn moves
(defun twostep-blackpawn-moves (b)
  (if (= (color b) black)
      (let* ((pawns (aref (boards b) black PAWN))
	     (secondrow (logand pawns (aref rankBits 6)))
	     (notblocker (lognot (blocker b)))
	 (movedpawns (logand (ash secondrow 8) notblocker)))
	(setf movedpawns (logand (ash movedpawns 8) notblocker))
	(do-bits (cord movedpawns)
		 (collect (newMove (+ cord 16) cord))))))

;; generate capture left black pawn moves
(defun captureleft-blackpawn-moves (b)
  (if (= (color b) black)
      (let* ((enpassants (enpassant b))
	     (enemies (getEnemies b black))
	     (pawns (aref (boards b) black PAWN))
	     (capLeftPawns (logand pawns (lognot (aref fileBits 7))))
	     (pawnEnemies (logior enemies (or (and (not (null enpassants)) 
						   (aref bitPosArray enpassants)) 0))))
	(setf capLeftPawns (logand (ash capLeftPawns 7) pawnEnemies))
	(apply 'append 
	       (do-bits (cord capLeftPawns)
			(collect 
			 (cond ((>= cord 56) (loop for p being the elements of PROMOTIONS
						   collect (newMove (+ cord 7) cord p)))
			       ((equal cord enpassants) (list (newMove (+ cord 7) cord ENPASSANT)))
			       (t (list (newMove (+ cord 7) cord))))))))))

;; generate capture right black pawn moves
(defun captureright-blackpawn-moves (b)
  (if (= (color b) black)
      (let* ((enpassants (enpassant b))
	     (enemies (getEnemies b black))
	     (pawns (aref (boards b) black PAWN))
	     (capLeftPawns (logand pawns (lognot (aref fileBits 0))))
	     (pawnEnemies (logior enemies (or (and (not (null enpassants)) 
						   (aref bitPosArray enpassants)) 0))))
	(setf capLeftPawns (logand (ash capLeftPawns 9) pawnEnemies))
	(apply 'append 
	       (do-bits (cord capLeftPawns)
			(collect 
			 (cond ((>= cord 56) (loop for p being the elements of PROMOTIONS
						   collect (newMove (+ cord 9) cord p)))
			       ((equal cord enpassants) (list (newMove (+ cord 9) cord ENPASSANT)))
			       (t (list (newMove (+ cord 9) cord))))))))))
  
;; generate castling for one side
(defun generateOne (b color rooknum king-after rook-after)
  (let ((castle KING_CASTLE))
    (if (= rooknum 0)
	(setf castle QUEEN_CASTLE))
    (let* ((kings (aref (ini-kings b) color))
	   (rooks (aref (aref (ini-rooks b) color) rooknum))
	   (blocker (returnWithClearBit (returnWithClearBit (blocker b) kings) rooks))
	   (stepover (logior (aref fromToRay kings king-after)
			     (aref fromToRay rooks rook-after)))
	   (attacked '()))
  
      (if (= (logand stepover blocker) 0)
	  (progn
	    (loop for cord from (min kings king-after) to (+ (max kings king-after) 1) do
		(if (isAttacked b cord (- 1 color)) (setq attacked t)))
	    (if (not attacked) (newMove kings king-after castle)))))))

;; generate castle moves
(defun genCastles (b)
  (let ((moves '()))
    (if (= (color b) white)
	(progn
	  (if (not-zero (logand (castling b) W_OO))
	      (let ((move (generateOne b WHITE 1 G1 F1)))
		(if move (setf moves (cons move moves)))))
	  (if (not-zero (logand (castling b) W_OOO))
	      (let ((move (generateOne b WHITE 0 C1 D1)))
		(if move (setf moves (cons move moves))))))
      (progn
	(if (not-zero (logand (castling b) B_OO))
	    (let ((move (generateOne b BLACK 1 G8 F8)))
	      (if move (setf moves (cons move moves)))))
	(if (not-zero (logand (castling b) B_OOO))
	    (let ((move (generateOne b BLACK 0 C8 D8)))
	      (if move (setf moves (cons move moves)))))))
    moves))
  
;; generate moves that captures	
(defun genCaptures (b)
  (let ((enemies (getEnemies b (color b))))
    (append
     (gen-knight-moves b enemies)
     (gen-king-moves b enemies)
     (gen-rooque-moves b enemies)
     (gen-bisque-moves b enemies)
     (captureleft-whitepawn-moves b)
     (captureright-whitepawn-moves b)
     (captureleft-blackpawn-moves b)
     (captureright-blackpawn-moves b))))

;; generate moves that doesn't capture
(defun genNonCaptures (b)
  (let ((notblocker (lognot (blocker b))))
    (append
     (gen-knight-moves b notblocker)
     (gen-king-moves b notblocker)
     (gen-rooque-moves b notblocker)
     (gen-bisque-moves b notblocker)
     (onestep-whitepawn-moves b)
     (twostep-whitepawn-moves b)
     (onestep-blackpawn-moves b)
     (twostep-blackpawn-moves b)
     (genCastles b))))

;;generate all possible moves except when king is in check
(defun generate-all-moves (b)
  (let ((notfriends (lognot (aref (friends b) (color b)))))
    (append
     (gen-knight-moves b notfriends)     
     (gen-king-moves b notfriends)
     (gen-rooque-moves b notfriends)
     (gen-bisque-moves b notfriends)
     (onestep-whitepawn-moves b)
     (twostep-whitepawn-moves b)
     (captureleft-whitepawn-moves b)
     (captureright-whitepawn-moves b)  
     (onestep-blackpawn-moves b)
     (twostep-blackpawn-moves b)
     (captureleft-blackpawn-moves b)   
     (captureright-blackpawn-moves b)
     (genCastles b)
)))

;;captures checking pieces
(defun captures-checking-pieces (b checkers)
  (let* ((color (color b))
	 (chkcord (firstBit checkers))
	 (arBoard (arBoard b))
	 (kings (aref (boards b) color KING))
	 (attacks (logand (getAttacks b chkcord color) (lognot kings))))   
    (do-bits (cord attacks)
	     (collect
	      (if (not (pinnedOnKing b cord color))
		  (if (and (= (aref arBoard cord) PAWN)
			   (or (<= chkcord H1) (>= chkcord A8)))
		      (loop for p being the elements of PROMOTIONS
			    collect (newMove cord chkcord p))
		    (list (newMove cord chkcord))))))))
  
;;capture using enpassant
(defun enpassant-capture (b checkers)
  (let* ((chkcord (firstBit checkers))
	 (color (color b))
	 (pawns (aref (boards b) color PAWN)))
    (if (enpassant b)
	      (let ((ep (enpassant b)))
		(if (= (+ ep (or (and (= color WHITE) -8) 8) chkcord))
		    (let ((bits (logand 
				 (move-array (or (and (= color WHITE) BPAWN) PAWN) ep)
				 pawns)))
		      (do-bits (cord bits)
			       (if (not (pinnedOnKing b cord color))
				   (collect (newMove cord ep ENPASSANT))))))))))

;;get the moves that will block pieces. This function will be used in block checking piece
(defun get-block-moves (b attacks checkers cord)
  (let ((color (color b))
	(arBoard (arBoard b))
	(chkcord (firstBit checkers)))
    (do-bits (fcord attacks)
	     (collect
	      (if (not (pinnedOnKing b fcord color))
		  (if (and (= (aref arBoard fcord) PAWN)
			   (or (<= chkcord H1) (>= chkcord A8)))
		      (loop for p being the elements of PROMOTIONS
			    collect (newMove fcord cord p))
		    (list (newMove fcord cord))))))))

;;generate moves that can block checking pieces
(defun block-checking-piece (b)
  (let* ((color (color b))
	 (opcolor (- 1 color))
	 (kcord (aref (kings b) color))
	 (kings (aref (boards b) color KING))
	 (pawns (aref (boards b) color PAWN))
	 (checkers (getAttacks b kcord opcolor))
	 (arBoard (arBoard b))
	 (chkcord (firstBit checkers)))
    (if (nth (aref arBoard chkcord) sliders)
	(let ((bits (returnWithClearbit 
		     (aref fromToRay kcord chkcord) chkcord)))
	  (do-bits (cord bits)
		 (let* ((a (getAttacks b cord color))
			(a (logand a (lognot (logior kings pawns)))))
		   (cond ((and (= color WHITE) (> cord H2))
			  (if (not-zero (logand (aref bitPosArray (- cord 8)) pawns))
			      (setf a (logior a (aref bitPosArray (- cord 8)))))
			  (if (and 
			       (= (ash cord -3) 3)
			       (= (aref arBoard (- cord 8)) EMPTY)
			       (not-zero (logand (aref bitPosArray (- cord 16)) pawns)))
			      (setf a (logior a (aref bitPosArray (- cord 16))))))
			 ((and (= color BLACK) (< cord H7))
			  (if (not-zero (logand (aref bitPosArray (+ cord 8)) pawns))
			      (setf a (logior a (aref bitPosArray (+ cord 8)))))
			  (if (and 
			       (= (ash cord -3) 4)
			       (= (aref arBoard (+ cord 8)) EMPTY)
			       (not-zero (logand (aref bitPosArray (+ cord 16)) pawns)))
				(setf a (logior a (aref bitPosArray (+ cord 16)))))))
		   (collect (get-block-moves b a checkers cord))))))))

;;generate escape routes
(defun get-escape-route (b)
  (let* ((color (color b))
	 (opcolor (- 1 color))
	 (kcord (aref (kings b) color))
	 (checkers (getAttacks b kcord opcolor))
	 (arBoard (arBoard b))
	 (escapes (logand (move-array KING kcord)
			  (lognot (aref (friends b) color)))))
    (if (zero checkers)
	(setf escapes 0))
    (do-bits (chkcord checkers)
	     (let ((dir (aref directions chkcord kcord)))
	       (if (nth (aref arBoard chkcord) sliders)
		   (setf escapes (logand escapes (lognot (aref rays chkcord dir)))))))
    (do-bits (cord escapes)
	     (collect (if (not (isAttacked b cord opcolor))
			  (newMove kcord cord))))))

;;generate evasions for king if in check	
(defun genCheckEvasions (b)
  (let* ((color (color b))
	 (opcolor (- 1 color))
	 (kcord (aref (kings b) color))
	 (checkers (getAttacks b kcord opcolor)))
    (flatten (append
	      (if (= (bitLength checkers) 1)
		  (append
		   (captures-checking-pieces b checkers)
		   (enpassant-capture b checkers)
		   (block-checking-piece b)))
	      (get-escape-route b)))))

	  
			   

