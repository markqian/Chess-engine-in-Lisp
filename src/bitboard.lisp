
(defvar white-pawns 0)
(defvar white-king 0)
(defvar white-queen 0)
(defvar white-bishops 0)
(defvar white-rooks 0)
(defvar white-knights 0)

(defvar black-pawns 0)
(defvar black-king 0)
(defvar black-queen 0)
(defvar black-knights 0)
(defvar black-rooks 0)
(defvar black-bishops 0)

(defvar *count* 0)

(defmacro do-bits ((var x) &rest body)
  "Evaluates [body] forms after binding [var] to each set bit in [x]"
  (let ((k (gensym)))
    `(do ((,k ,x (logand ,k (1- ,k))))
         ((= ,k 0))
       (let ((,var (logand ,k (- ,k))))
         ,@body))))
 

(do-bits (cord knights) 
	 (do-bits (c (logand knightMoves notfriends))
		  (collect (newMove cord c))))


(defun increment-widget-count () (incf *count*))

(defvar index64 #(63 0 58 1 59 47 53 2
		  60 39 48 27 54 33 42 3
	          61 51 37 40 49 18 28 20
		  55 30 34 11 43 14 22 4
	          62 57 46 52 38 26 32 41
		  50 36 17 19 29 10 13 21
	          56 45 25 31 35 16 9 12
		  44 24 15  8 23 7 6 5))


(defun bitScanForward (bb) (let* ((debruijn64 #x07EDD5E59A4E28C2)
				  (position (mod (ash (* (logand bb (- bb)) debruijn64) -58) 64)))
			     (aref index64 position)))


(defun trailingZeroCount (bb) 
 (let ((lookup67 '(64 0 1 39 2 15 40 23
		   3 12 16 59 41 19 24 54
		   4 -1 13 10 17 62 60 28
		   42 30 20 51 25 44 55 47
		   5 32 -1 38 14 22 11 58
		   18 53 63 9 61 27 29 50
		   43 46 31 37 21 57 52 8
		   26 49 45 36 56 7 48 35
		   6 34 33 -1)))
 (nth (mod (logand bb (- bb)) 67) lookup67)))


(defun bitScanReverse (bb) 
 (let ((result 0)
       (bbtemp bb))
  (cond ((> bbtemp #xFFFFFFFF) 
	 (progn (setf bbtemp (ash bbtemp -32)) (setf result (+ result 32))))
	((> bbtemp #xFFFF) 
	 (progn (setf bbtemp (ash bbtemp -16)) (setf result (+ result 16))))
	((> bbtemp #xFF) 
	 (progn (setf bbtemp (ash bbtemp -8)) (setf result (+ result 8)))))
 (+ result (floor (log bbtemp 2)))))


(defparameter *white-pieces* '((pawn 8) (pawn 9) (pawn 10) (pawn 11) (pawn 12) (pawn 13) (pawn 14) (pawn 15) (king 3) (queen 4) (bishop 2) (bishop 5) (rook 7) (rook 0) (knight 1) (knight 6)))

(defparameter *black-pieces* '((pawn 48) (pawn 49) (pawn 50) (pawn 51) (pawn 52) (pawn 53) (pawn 54) (pawn 55) (king 59) (queen 60) (bishop 58) (bishop 61) (rook 56) (rook 63) (knight 57) (knight 62)))

(defun set2! (index integer)
  (setf (ldb (byte 1 index) integer) 1))

(defmacro set! (index integer)
  `(setf (ldb (byte 1 ,index) ,integer) 1))


(defun logbit (index integer)
  (ldb (byte 1 index) integer))

(defun set-cord (p color coord) 
  (cond ((eq (car p) 'pawn) (if (eq 'white color) (setf (logbit coord white-pawns) 1) (setf (logbit coord black-pawns) 1)))
	((eq (car p) 'king) (if (eq 'white color) (setf (logbit coord white-king) 1) (setf (logbit coord black-king) 1)))
	((eq (car p) 'queen) (if (eq 'white color) (setf (logbit coord white-queen) 1) (setf (logbit coord black-queen) 1)))
	((eq (car p) 'rook) (if (eq 'white color) (setf (logbit coord white-rooks) 1) (setf (logbit coord black-rooks) 1)))
	((eq (car p) 'knight) (if (eq 'white color) (setf (logbit coord white-knights) 1) (setf (logbit coord black-knights) 1)))
	((eq (car p) 'bishop) (if (eq 'white color) (setf (logbit coord white-bishops) 1) (setf (logbit coord black-bishops) 1)))))
       
(defun applyfun (func lst)
  (if (null lst) t
    (progn (funcall func (car lst)) (applyfun func (cdr lst)))))
  

(defun set-pieces () 
  (progn 
    (applyfun #'(lambda (pair) (set-cord pair 'black (second pair))) *black-pieces*)
    (applyfun #'(lambda (pair) (set-cord pair 'white (second pair))) *white-pieces*)))


(defvar white-pieces (logior white-pawns white-king white-queen white-bishops white-knights white-rooks))
 
(defvar black-pieces (logior black-pawns black-king black-queen black-bishops black-knights black-rooks))

(defvar occupied-squares (logior white-pieces black-pieces))

(defvar empty-squares (lognot occupied-squares))

(defun rank-mask (sq) (ash #x0ff (logand sq 56)))
(defun file-mask (sq) (ash #x0101010101010101 (logand sq 7)))


(defun diagonal-mast (sq) (let* ((maindia #x8040201008040201)
          (diag (- (* 8 (logand sq 7)) (logand sq 56)))
	  (nort (logand (- diag) (ash diag -31)))
	  (sout (logand diag (ash (- diag) -31))))
    (ash (ash maindia (- sout)) nort)
  ))

(defun knightAttacks(knights) (let* ((l1 (logand (ash knights -1) #x7f7f7f7f7f7f7f7f)) 
				     (l2 (logand (ash knights -2) #x3f3f3f3f3f3f3f3f))
				     (r1 (logand (ash knights 1) #xfefefefefefefefe))
				     (r2 (logand (ash knights 2) #xfcfcfcfcfcfcfcfc))
				     (h1 (logior l1 r1))
				     (h2 (logior l2 r2)))
				(logior (ash h1 16) (ash h1 -16) (ash h2 8) (ash h2 -8))))

(defun knightFill(knights) (logior (knightAttacks knights) knights)) 

(defun forkTargetSquare(targets) ()) ;;at Knight+Pattern

(defun eastOne (board) ())
(defun westOne (board) ())
(defun soutOne (board) ())
(defun northOne (board) ())

(defun kingAttacks(kingSet) (let* ((attacks (logior (eastOne kingset) (westOne kingSet)))
				   (kingSet2 (logior kingSet attacks))
				   (attacks2 (logior attacks (logior (northOne kingSet2) (soutOne kingSet2)))))
			      attacks2))

(defun north-west (bitboard) (if (not (edge)) (setf (logbit  bitboard ))
(defun north (bitboard) (#x0101010101010100))
(defun north-east (bitboard n) ())
(defun west (bitboard n) ())
(defun east (bitboard n) ())
(defun south-west (bitboard n) ())
(defun south (bitboard n) ())
(defun south-east (bitboard n) ())

(defun knight-moves (board) board)
(defun knight-right-corner-moves (board) (+ board 8))
(defun knight-left-corner-moves (board) board)
(defun knight-left-corner-moves (board) board)
(defun knight-left-corner-moves (board) board)
(defun bishop-moves (board) board)
(defun bishop-ray (board) board)
(defun rook-moves (board) board)
(defun pawn-moves (board) board)
(defun queen-moves (board) board)
(defun king-moves (board) board)

;;use hashtable to store values
(defun bitscan (bitboard reversed) (bitboard))

(defun ray-attacks (bitboard direction occupied) ())
(defun get-8-bits (bits n) 
  (if (> n 8) nil
    (cons (car bits) (get-8-bits (cdr bits) (+ n 1)))))

(defun get-rest (bits n)
  (if (> n 8) bits
    (get-rest (cdr bits) (+ n 1))))

(defun divide-bits (bits n)
  (if (> n 8) nil
    (cons (reverse (get-8-bits bits 1)) (divide-bits (get-rest bits 1) (+ n 1)))))

(defun make-bit-list (bits i)
  (cond ((< i 0) nil)
        ((logbitp i bits) (cons 1 (make-bit-list bits (- i 1))))
	(t (cons 0 (make-bit-list bits (- i 1))))))

(defun print-board (board) 
   (format nil "~:{~% ~D ~D ~D ~D ~D ~D ~D ~D~}" (divide-bits (make-bit-list board 63) 1)))

