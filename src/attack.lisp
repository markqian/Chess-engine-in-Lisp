(load "./src/board.lisp")

(defun zero (i)
  (= i 0))

(defun not-zero (i)
  (not (zero i)))

(defun pboards (b piece color)
  (aref (boards b) color piece))

(defun move-array (piece cord)
  (aref moveArray piece cord))

(defun pieces (b color piece)
  (aref (boards b) color piece))

;;generate bishop and queen attack on the specified coordinate
(defun bisque-attacks (b cord color)
  (let* ((bisque (logand (logior (pboards b BISHOP color)
				 (pboards b QUEEN color))
			 (move-array BISHOP cord)))
	 (others (logand (lognot bisque) (blocker b))))
    (do-bits (bit bisque)
	     (let ((ray (aref fromToRay cord bit)))
	       (if (and (not (= ray 0)) (= (logand ray others) 0)) (ret t))))))
  
;;generate rook and queen attacks on the specified coordinate
(defun rooque-attacks (b cord color)
  (let* ((rooque (logand (logior (pboards b ROOK color)
				 (pboards b QUEEN color))
			  (move-array ROOK cord)))
	 (others (logand (lognot rooque) (blocker b))))
    (do-bits (bit rooque)
	     (let ((ray (aref fromToRay cord bit)))
	       (if (and (not (= ray 0)) (= (logand ray others) 0)) (ret t))))))
  
;;check if a particular coordinate is attacked
(defun isAttacked (b cord color)
  (let ((ptype (or (and (= color white) BPAWN) PAWN)))
    (cond
     ((not (zero (logand (pboards b KNIGHT color) (move-array KNIGHT cord))))
      T)
     ((bisque-attacks b cord color) T)
     ((rooque-attacks b cord color) T)
     ((not (zero (logand (pboards b PAWN color) (move-array ptype cord))))
      T)    
     ((not (zero (logand (pboards b KING color) (move-array KING cord))))
      T))))
  
;;Get all attacks on the particular cordinate
(defun getAttacks (b cord color)
  (let* ((bits (logand (pieces b color KNIGHT) 
		      (move-array KNIGHT cord)))
	 (bits (logior bits 
			(logand (pieces b color KING)
				(move-array KING cord))))
	 (bits (logior bits 
			(logand (pieces b color PAWN)
				(move-array (or (and (= color WHITE) BPAWN) PAWN)
					    cord))))
	 (blocker (blocker b)))
    (let ((bisque (logand (logior (pieces b color BISHOP)
				 (pieces b color QUEEN))
			 (move-array BISHOP cord))))
      (do-bits (c bisque)
	       (let ((ray (aref fromToRay cord c)))
		 (if (and ray (zero (returnWithClearbit (logand ray blocker) c)))
		     (setf bits (logior bits (aref bitPosArray c)))))))

    (let ((rooque (logand (logior (pieces b color ROOK)
				 (pieces b color QUEEN))
			 (move-array ROOK cord))))
      (do-bits (c rooque)
	       (let ((ray (aref fromToRay cord c)))
		 (if (and ray (zero (returnWithClearbit (logand ray blocker) c)))
		     (setf bits (logior bits (aref bitPosArray c)))))))
    bits))

;;Generate a bitboard and return all possible moves of the piece
(defun getPieceMoves (b cord color piece)
  (let ((blocker (blocker b))
	(bits 0))
    (if (or (= piece KNIGHT) (= piece KING))
	(logand (pieces b color piece) (move-array piece cord)))

    (if (nth piece sliders)
	(let ((cords (logand (pieces b color piece) (move-array piece cord)))
	      (bits 0))
	  (do-bits (c cords)
		   (let ((ray (aref fromToRay cord c)))
		     (if (and ray (not (returnWithClearbit (logand ray blocker) c)))
			 (progn
			   (setf bits (logior bits (aref bitPosArray c)))
			   bits))))))

    (if (= piece PAWN)
	(let* ((pawns (pieces b color PAWN))
	       (bits (logand pawns 
			     (move-array (or (and (= color WHITE) BPAWN) PAWN) cord)))
	       (bits (logior bits 
			     (logand pawns (aref bitPosArray 
						 (+ cord (or (and (= color WHITE)
								  -8) 8)))))))
	  (if (not (logand blocker (aref bitPosArray (+ cord 
							(or (and (= color WHITE)
								 -8) 8)))))
	      (setf bits (logior bits (logand pawns (aref rankBits (or (and
									(= color WHITE)
									1) 6))))))))
    bits))



;;check if there is the pin on the king
(defun pinnedOnKing (b cord color)
  (let* ((kingCord (aref (kings b) color))
	 (dir (aref directions kingCord cord))
	 (opcolor (- 1 color))
	 (blocker (blocker b))
	 (bv (if (= dir -1) 0 
	       (logand (logxor (aref rays kingCord dir) 
			       (aref fromToRay kingCord cord))
		       blocker)))
	 (cord1 (if (zero bv) 0 
		  (if (> cord kingCord) (firstBit bv)
		    (lastBit bv)))))

    (cond
     ((= dir -1)
      nil)
     ((not (zero (logand (returnWithClearbit (aref fromToRay kingCord cord) cord)
			 blocker)))
      nil)     
     ((zero bv) nil)
     ((and (<= dir 3) 
	   (not (zero (logand (aref bitPosArray cord1) 
			      (logior (aref (boards b) opcolor QUEEN)
				      (aref (boards b) opcolor BISHOP))))))
      T)
     ((and (>= dir 4) 
	   (not (zero (logand (aref bitPosArray cord1) 
			      (logior (aref (boards b) opcolor QUEEN)
				      (aref (boards b) opcolor ROOK))))))
      T)
     (T nil))))

(defvar xray '(() T () T T T ()))

(defun addXrayPiece (b tcord fcord color ours theirs)
  (let ((dir (aref directions tcord fcord))
	(a (logand (aref rays fcord dir) (blocker b)))
	(our 0))
    (if (not a) (list our theirs))

    (let ((ncord (firstBit a)))
      (if (> tcord fcord)
	  (setf ncord (lastBit a)))
      (let ((piece (aref (arBoard b) ncord)))
	(if (or (= piece QUEEN) (or (and (= piece ROOK) (> dir 3))
				    (and (= piece BISHOP) (< dir 4))))
	    (let ((bit (aref bitPosArray ncord)))
	      (if (logand bit (aref (friends b) color))
		  (setf ours (logior ours bit))
		(setf theirs (logior theirs bit)))))))
    (list ours theirs)))

;;Determines whether the current player is in check	             
(defun isChecked (b)   
  (if (null (checked b)) (let ((kingcord (aref (kings b) 
					       (color b))))
			   (setf (checked b) 
				 (isAttacked b kingcord 
					     (- 1 (color b))))))
  (checked b))

;;Determines whether opponent is in check
(defun opIsChecked (b)
  (if (null (opchecked b)) (let ((kingcord 
				(aref (kings b) 
				      (- 1 (color b)))))
			   (setf (opchecked b) 
				 (isAttacked b kingcord 
					     (color b)))))
  (opchecked b))
