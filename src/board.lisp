(load "~/chess/src/utility.lisp")

(defun reset-board (b)
  (setf (blocker b) (createBoard 0)
        (friends b) (make-array '(2) :initial-element 0)
        (kings b) (make-array '(2) :initial-element -1)
        (boards b) (make-array '(2 7) :initial-element 0)
        (enpassant b) -1
        (color b) WHITE
        (castling b) (logior B_OOO B_OO W_OOO W_OO)
        (hasCastled b) (make-array '(2) :initial-element nil)
        (fifty b) 0
        (checked b) nil
        (opchecked b) nil
        (arBoard b) (make-array '(64) :initial-element 0)
        (hash b) 0
        (pawnhash b) 0
        (history b) '()))

(defun addPiece (b cord piece color)
  (setbit-board b color piece cord)
  (cond ((= piece KING) 
	 (set-kings b color cord)))
  (setpieceHash b color piece cord)
  (set-arBoard b cord piece))


(defun removePiece (b cord piece color)
  (clearBit (aref (boards b) color piece) cord)
  (setpieceHash b color piece cord)
  (set-arBoard b cord EMPTY))


(defun move (b fcord tcord piece color)
  (removePiece b fcord piece color)
  (addPiece b tcord piece color))

(defun combine-element (f v c n)
  (if (= n 0) (funcall f (aref v c 0))
    (funcall f (aref v c n) (combine-element f v c (- n 1)))))
  

(defun updateBoard (b)
  (setf (aref (friends b) WHITE) (combine-element #'logior (boards b) WHITE 6))
  (setf (aref (friends b) BLACK) (combine-element #'logior (boards b) BLACK 6))
  (setf (blocker b) (logior (aref (friends b) WHITE) (aref (friends b) BLACK))))

(defun setColor (b color)
  (if (= color (color b)) nil
    (setf (color b) color))
  (setf (hash b) (logior (hash b) colorHash))
  (setf (pawnhash b) (logior (pawnhash b) colorHash)))


(defun setCastling (b castling)
  (if (= (castling b) castling) T)
  (if (= (logand (castling b) W_OO) castling) 
      (setf (hash b) (logior (hash b) W_OOHash)))
  (if (= (logand (castling b) W_OOO) castling) 
      (setf (hash b) (logior (hash b) W_OOOHash)))
  (if (= (logand (castling b) B_OO) castling) 
      (setf (hash b) (logior (hash b) B_OOHash)))
  (if (= (logand (castling b) B_OOO) castling) 
      (setf (hash b) (logior (hash b) B_OOOHash)))
  (setf (castling b) castling))

(defun setEnpassant (b epcord)
  (if (equal (enpassant b) epcord) T)
  (if (not (eq (enpassant b) nil)) 
      (setf (hash b) (logior (hash b) (aref epHashes (mod (enpassant b) 64)))))
  (if (not (eq epcord nil))
      (setf (hash b) (logior (hash b) (aref epHashes epcord))))
  (setf (enpassant b) epcord))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if
			 #'(lambda (c)
			     (not (funcall test c)))
			 str :start p1)))		 
		 (cons (subseq str p1 p2)
		       (if p2
			   (tokens str test p2)
			 nil)))
	    nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun not-slash (c)
  (not (equal c #\/)))

(defun map-null (n)
  (let ((acc nil))
    (dotimes (i n)
      (push '() acc))
    acc))

(defun parse-piece-placement (b pieceChrs)
  (let ((r 0))
    (dolist (rank (tokens pieceChrs #'not-slash 0))
	    (let ((cord (* (- 7 r) 8)))
	      (incf r)
	      (loop for char across rank do
		    (if (digit-char-p char)
			(incf cord (digit-char-p char))
		      (let ((color (or (and (lower-case-p char) BLACK) WHITE))
			    (piece (position (string (char-upcase char))
					     reprSign :test #'equal)))
			(addPiece b cord piece color)
			(incf cord))))))))

(defun parse-color-field (b colChr)
  (if (equal (string-downcase colChr) "w")
      (setColor b WHITE)
    (setColor b BLACK)))

(defun parse-castle-availability (b castChr)
  (let ((castling 0))
    (loop for char across castChr do
	  (cond ((equal char #\K) (bitincf castling 'logior W_OO))
		((equal char #\Q) (bitincf castling 'logior W_OOO))
		((equal char #\k) (bitincf castling 'logior B_OO))
		((equal char #\q) (bitincf castling 'logior B_OOO)))
	  (setCastling b castling))))

(defun parse-enpassant (b epChr)
  (if (equal epChr "-")
      (setEnpassant b nil)
    (setEnpassant b (cdr (assoc epChr cordDic :test 'equal)))))

(defun parse-halfmove-clockfield (b fiftyChr)
  (setf (fifty b) (max (parse-integer fiftyChr) 0)))

(defun parse-fullmove-number (b moveNoChr)
  (let ((movenumber (- (* (parse-integer moveNoChr) 2) 2)))
    (if (= (color b) BLACK) (incf movenumber))
    (setf (history b) (map-null movenumber))
    (updateBoard b)))

    
(defun applyFen (b fenstr)
  (let* ((parts (tokens fenstr #'constituent 0))
	 (pieceChrs (nth 0 parts))
	 (colChr (nth 1 parts))
	 (castChr (nth 2 parts))
	 (epChr (nth 3 parts))
	 (fiftyChr (if (or (>= (length parts) 6) (= (length parts) 5))
		       (nth 4 parts) "0"))
	 (moveNoChr (if (>= (length parts) 6) (nth 5 parts) "1")))
    ;; (if (not (validate-slashes pieceChrs)) nil)
    (reset-board b)
    (parse-piece-placement b pieceChrs)
    (parse-color-field b colChr)
    (parse-castle-availability b castChr)
    (parse-enpassant b epChr)
    (parse-halfmove-clockfield b fiftyChr)
    (parse-fullmove-number b moveNoChr)
    b))


(defun update-history (b move tpiece)
  (setf (history b) (cons  
		     (list move 
			   tpiece 
			   (enpassant b) 
			   (castling b)
			   (hash b) 
			   (fifty b) 
			   (checked b) 
			   (opchecked b))
		     (history b))))


(defun castling-move (b move)
  (let* ((flag (ash move -12))
	 (fcord (logand (ash move -6) 63)))
    (if (find flag `(,king_castle ,queen_castle))
	(progn
	  (if (= flag queen_castle)
	      (let ((rookf (- fcord 4))
		    (rookt (- fcord 1)))
		(move b rookf rookt ROOK (color b)))
	    (let ((rookf (+ fcord 3))
		  (rookt (+ fcord 1)))
	      (move b rookf rookt ROOK (color b))
	      ))
	  (setf (aref (hasCastled b) (color b)) T)))))
    

(defun capture-piece (b move) 
  (let* ((flag (ash move -12))
	 (fcord (logand (ash move -6) 63))
	 (tcord (logand move 63))
	 (fpiece (aref (arBoard b) fcord))
	 (tpiece (aref (arBoard b) tcord))
	 (opcolor (- 1 (color b))))

    (if (not (= tpiece EMPTY))
	(removePiece b tcord tpiece opcolor))
    
    (if (= fpiece PAWN)
	(cond ((= flag ENPASSANT)
	       (let ((takenPawnC (+ tcord (or (and (= (color b) white) -8) 8))))
		 (removePiece b takenPawnC PAWN opcolor)))
	      ((find flag PROMOTIONS)
	       (let ((piece (- flag 2)))
		 (removePiece b fcord PAWN (color b))
		 (addPiece b tcord piece (color b))))))

    (if (and (= fpiece PAWN) (= (abs (- fcord tcord)) 16))
	(setEnpassant b (/ (+ fcord tcord) 2))
      (setEnpassant b '()))
    
    (if (and (= tpiece EMPTY) (not (= fpiece PAWN)) 
	     (not (find flag `(,king_castle ,queen_castle))))
	(incf (fifty b))
      (setf (fifty b) 0))))
  
(defun getrooks (b c p)
  (aref (aref (ini-rooks b) c) p))

(defun clear-castle-flags (b fpiece tpiece fcord tcord)
  (if (= (color b) WHITE)
      (progn
	(if (= fpiece KING)
	    (progn
	      (if (logand (castling b) W_OOO)
		  (progn
		    (bitincf (hash b) 'logxor W_OOOHash)
		    (bitincf (castling b) 'logand (lognot W_OOO))))
	      (if (logand (castling b) W_OO)
		  (progn
		    (bitincf (hash b) 'logxor W_OOHash)
		    (bitincf (castling b) 'logand (lognot W_OO))))))
	(if (= fpiece ROOK)
	      (cond ((= fcord (getrooks b 0 1))
		     (if (logand (castling b) W_OO)
			 (progn
			   (bitincf (hash b) 'logxor W_OOHash)
			   (bitincf (castling b) 'logand (lognot W_OO)))))
		    ((= fcord (getrooks b 0 0))
		     (if (logand (castling b) W_OO)
			 (progn
			   (bitincf (hash b) 'logxor W_OOOHash)
			   (bitincf (castling b) 'logand (lognot W_OOO)))))))
	(if (= tpiece ROOK)
	      (cond ((= tcord (getrooks b 1 1))
		     (if (logand (castling b) B_OO)
			 (progn
			   (bitincf (hash b) 'logxor B_OOHash)
			   (bitincf (castling b) 'logand (lognot B_OO)))))
		    ((= tcord (getrooks b 1 0))
		     (if (logand (castling b) B_OOO)
			 (progn
			   (bitincf (hash b) 'logxor B_OOOHash)
			   (bitincf (castling b) 'logand (lognot B_OOO))))))))
    (progn
      (if (= fpiece KING)
	  (progn
	    (if (logand (castling b) B_OOO)
		(progn
		  (bitincf (hash b) 'logxor B_OOOHash)
		  (bitincf (castling b) 'logand (lognot B_OOO))))
	    (if (logand (castling b) B_OO)
		(progn
		  (bitincf (hash b) 'logxor B_OOHash)
		  (bitincf (castling b) 'logand (lognot B_OO))))))
      (if (= fpiece ROOK)	  
	  (cond ((= fcord (getrooks b 1 1))
		 (if (logand (castling b) B_OO)
		     (progn
		       (bitincf (hash b) 'logxor B_OOHash)
		       (bitincf (castling b) 'logand (lognot B_OO)))))
		((= fcord (getrooks b 1 0))
		 (if (logand (castling b) B_OO)
		     (progn
		       (bitincf (hash b) 'logxor B_OOOHash)
		       (bitincf (castling b) 'logand (lognot B_OOO)))))))
      (if (= tpiece ROOK)	  
	  (cond ((= tcord (getrooks b 0 1))
		 (if (logand (castling b) W_OO)
		     (progn
		       (bitincf (hash b) 'logxor W_OOHash)
		       (bitincf (castling b) 'logand (lognot W_OO)))))
		((= tcord (getrooks b 0 0))
		 (if (logand (castling b) W_OOO)
		     (progn
		       (bitincf (hash b) 'logxor W_OOOHash)
		       (bitincf (castling b) 'logand (lognot W_OOO))))))))))

(defun not-promotions (b fpiece fcord tcord flag)
  (if (not (find flag promotions))
      (progn	
	(move b fcord tcord fpiece (color b)))))

(defun applyMove (b move)
  (let* ((flag (ash move -12))
	 (fcord (logand (ash move -6) 63))
	 (tcord (logand move 63))
	 (fpiece (aref (arBoard b) fcord))
	 (tpiece (aref (arBoard b) tcord))
	 (opcolor (- 1 (color b))))
    (update-history b move tpiece)
    (setf (opchecked b) nil)
    (setf (checked b) nil)
    (capture-piece b move)
    (castling-move b move)
    (clear-castle-flags b fpiece tpiece fcord tcord)
    (not-promotions b fpiece fcord tcord flag)
    (setColor b opcolor)
    (updateBoard b)
))

(defun reverse-rook-move (b flag fcord color)
  (if (find flag `(,king_castle ,queen_castle))
      (let ((rookf 0)
	    (rookt 0))
	(if (= flag queen_castle)
	    (setf rookf (- fcord 4)
		  rookt (- fcord 1))
	  (setf rookf (+ fcord 3)
		rookt (+ fcord 1)))
	(move b rookt rookf ROOK color)
	(setf (aref (hasCastled b) color) nil))))

(defun reverse-captured-piece/square (b flag tcord fcord cpiece tpiece color)
  (let ((opcolor (- 1 color)))
    (cond ((not (= cpiece empty))
	   (if (find flag promotions)
	       (progn
		 (addPiece b tcord cpiece opcolor)
		 (addPiece b fcord pawn color))
	     (progn
	       (addPiece b tcord cpiece opcolor)
	       (addPiece b fcord tpiece color))))
	  ((= flag enpassant)
	   (let ((epcord (or (and (= color white) (- tcord 8)) (+ tcord 8))))
	     (addPiece b epcord pawn opcolor)
	     (addPiece b fcord pawn color)))
	  ((find flag promotions)
	   (addPiece b fcord pawn color))
	  (t (addPiece b fcord tpiece color)))))
  

(defun popMove (b)
  (let* ((last-history (car (history b)))
	 (color (- 1 (color b)))
	 (move (nth 0 last-history))
	 (flag (ash move -12))
	 (fcord (logand (ash move -6) 63))
	 (tcord (logand move 63))
	 (tpiece (aref (arBoard b) tcord))
	 (cpiece (nth 1 last-history))
	 (enpassants (nth 2 last-history))
	 (castling (nth 3 last-history))
	 (hash (nth 4 last-history))
	 (fifty (nth 5 last-history))
	 (checked (nth 6 last-history))
	 (opchecked (nth 7 last-history)))
    
    (removePiece b tcord tpiece color)
    (reverse-rook-move b flag fcord color)
    (reverse-captured-piece/square b flag tcord fcord cpiece tpiece color)

    (setColor b color)
    (updateBoard b)
    (setf (checked b) checked
	  (opchecked b) opchecked
	  (enpassant b) enpassants
	  (castling b) castling
	  (hash b) hash
	  (fifty b) fifty
	  (history b) (cdr (history b)))))

    

(defvar test 42)
