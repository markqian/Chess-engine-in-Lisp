#!/usr/bin/sbcl --script
(load "~/chess/src/search.lisp")

;;board object
(defvar board1 (make-instance 'board))

;;apply init position fenstring to board1
(applyFen board1 init-pos)

;;helper functions  
(defun getPieceCode (move)
  (let ((m (subseq move 4 5)))
    (cond ((equal m "k")
	   (+ KNIGHT 2))
	  ((equal m "b")
	   (+ BISHOP 2))
	  ((equal m "r")
	   (+ ROOK 2))
	  ((equal m "q")
	   (+ QUEEN 2))
	  (t 0))))

(defun is-promotion (move)
  (= (length move) 5))

;;parsing xboard input
(defun convert-to-move (b move)
  (let* ((fcord
	  (position (subseq move 0 2) square-name :test #'equal))
	 (tcord 
	  (position (subseq move 2 4) square-name :test #'equal))
	 (fpiece (aref (arBoard b) fcord)))    
    (cond 
     ((is-promotion move)
      (newMove fcord tcord (getPieceCode move)))
     ((and (equal "e1g1" move) (equal (aref (arBoard b) 4) KING))
      (newMove fcord tcord KING_CASTLE))
     ((and (equal "e1c1" move) (equal (aref (arBoard b) 4) KING))
      (newMove fcord tcord QUEEN_CASTLE))
     ((and (equal "e8g8" move) (equal (aref (arBoard b) 60) KING))
      (newMove fcord tcord KING_CASTLE))
     ((and (equal "e8c8" move) (equal (aref (arBoard b) 60) KING))
      (newMove fcord tcord QUEEN_CASTLE))
     ((and (= fpiece PAWN) (equal tcord (enpassant b)))
      (newMove fcord tcord ENPASSANT))
   (t (newMove fcord tcord)))))


(defun getPieceString (p)
  (cond 
   ((= p 4) "k")
   ((= p 5) "b")
   ((= p 6) "r")
   ((= p 7) "q")))


(defun get-move-string (b)
  (let ((move (cadr (getResult b -1000 1000 3))))
    (if (equal move '())
	'()
      (if (>= (getflag move) 4)
	  (concatenate 'string 
		       (nth (getfrom move) square-name)
		       (nth (getto move) square-name)
		       (getPieceString (getflag move)))
	(concatenate 'string 
		     (nth (getfrom move) square-name)
		     (nth (getto move) square-name))))))

(defun get-move (b usrmove)
  (progn
   (applyMove b (convert-to-move b usrmove))
   (let ((move (get-move-string b)))
     (if (equal move '())
	 '()
       (progn
	 (applyMove b (convert-to-move b move))
	 move)))))

(defun c-strings (s)
  (if (equal s '()) s
    (concatenate 'string (car s) (c-strings (cdr s)))))

;;recieve command from xboard
(defun recieve-command (msg)
  (let ((tokens (tokenize-string msg)))
    (cond 
     ((equal (car tokens) "xboard") '())
     ((equal (car tokens) "protover") (write-line "feature usermove=1 sigint=0 sigterm=0")) 
     ((equal (car tokens) "accepted") '())
     ((equal (car tokens) "usermove")
      (let ((move (get-move board1 (cadr tokens))))
	(if move
	    (write-line (concatenate 'string "move " move))
	  (write-line "resign"))))
     (t '()))))

;;main loop for communicating with xboard and player. A file called output.txt is created to record xboard inputs for debugging purposes.
(loop
 (let ((x (read-line)))
   (let ((in (open "~/chess/output.txt" :if-does-not-exist :create :if-exists :append :direction :output)))
    (write-line x in)
    (if (equal x "quit") (return))
    (recieve-command x)
    (close in))))
