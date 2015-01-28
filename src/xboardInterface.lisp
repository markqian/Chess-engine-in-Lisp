#!/usr/bin/sbcl --script
(load "./src/search.lisp")

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun tokenize-string (str)
  (my-split str))

(defvar init-pos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defvar board1 (make-instance 'board))

(applyFen board1 init-pos)

(defvar square-name '("a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"
			"a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
			"a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3"
			"a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4"
			"a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5"
			"a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6"
			"a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
			"a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"))
  
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

(loop
 (let ((x (read-line)))
   (let ((in (open "~/chess/output.txt" :if-does-not-exist :create :if-exists :append :direction :output)))
    (write-line x in)
    (if (equal x "quit") (return))
    (recieve-command x)
    (close in))))
