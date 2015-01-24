(defmacro bitincf (bit f v)
  `(setf ,bit (funcall ,f ,bit ,v)))


(defun bitLength (x)
  (let ((count 0)
	(y x))
    (loop
     (if (= y 0) (return))
     (incf count)
     (bitincf y 'logand (- y 1)))
    count))

(defconstant NORMAL_MOVE 0)
(defconstant QUEEN_CASTLE 1)
(defconstant KING_CASTLE 2)
(defconstant ENPASSANT 3)
(defconstant KNIGHT_PROMOTION 4)
(defconstant BISHOP_PROMOTION 5)
(defconstant ROOK_PROMOTION 6)
(defconstant QUEEN_PROMOTION 7)

(defmacro setBit (b p)
  `(setf (ldb (byte 1 (- 63 ,p)) ,b) 1))

(defvar rankBits (make-array '(8)))
(defvar fileBits (make-array '(8)))

(do ((i 7 (- i 1)))
    ((< i 0))
      (setf (aref rankBits (- 7 i)) (ash 255 (* i 8)))
      (setf (aref fileBits (- 7 i)) (ash #x0101010101010101 i)))


(defconstant A1 0)
(defconstant B1 1)
(defconstant C1 2)
(defconstant D1 3)
(defconstant E1 4)
(defconstant F1 5)
(defconstant G1 6)
(defconstant H1 7)

(defconstant A2 8)
(defconstant B2 9)
(defconstant C2 10)
(defconstant D2 11)
(defconstant E2 12)
(defconstant F2 13)
(defconstant G2 14)
(defconstant H2 15)

(defconstant A3 16)
(defconstant B3 17)
(defconstant C3 18)
(defconstant D3 19)
(defconstant E3 20)
(defconstant F3 21)
(defconstant G3 22)
(defconstant H3 23)

(defconstant A4 24)
(defconstant B4 25)
(defconstant C4 26)
(defconstant D4 27)
(defconstant E4 28)
(defconstant F4 29)
(defconstant G4 30)
(defconstant H4 31)

(defconstant A5 32)
(defconstant B5 33)
(defconstant C5 34)
(defconstant D5 35)
(defconstant E5 36)
(defconstant F5 37)
(defconstant G5 38)
(defconstant H5 39)

(defconstant A6 40)
(defconstant B6 41)
(defconstant C6 42)
(defconstant D6 43)
(defconstant E6 44)
(defconstant F6 45)
(defconstant G6 46)
(defconstant H6 47)

(defconstant A7 48)
(defconstant B7 49)
(defconstant C7 50)
(defconstant D7 51)
(defconstant E7 52)
(defconstant F7 53)
(defconstant G7 54)
(defconstant H7 55)

(defconstant A8 56)
(defconstant B8 57)
(defconstant C8 58)
(defconstant D8 59)
(defconstant E8 60)
(defconstant F8 61)
(defconstant G8 62)
(defconstant H8 63)

(defconstant PROMOTIONS `#(,QUEEN_PROMOTION ,ROOK_PROMOTION ,BISHOP_PROMOTION ,KNIGHT_PROMOTION))

(defconstant EMPTY 0)
(defconstant PAWN 1)
(defconstant KNIGHT 2)
(defconstant BISHOP 3)
(defconstant ROOK 4)
(defconstant QUEEN 5)
(defconstant KING 6)
(defconstant BPAWN 7)


(defconstant BLACK 1)
(defvar maxint 100)
(defvar WHITE_SQUARES #x55AA55AA55AA55AA)
(defvar BLACK_SQUARES #xAA55AA55AA55AA55)
(defvar pieceHashes (make-array '(2 7 64) :initial-element 0))
(defvar colorHash 1)

(defvar reprCord '(
    "a1"  "b1"  "c1"  "d1"  "e1"  "f1"  "g1"  "h1" 
    "a2"  "b2"  "c2"  "d2"  "e2"  "f2"  "g2"  "h2" 
    "a3"  "b3"  "c3"  "d3"  "e3"  "f3"  "g3"  "h3" 
    "a4"  "b4"  "c4"  "d4"  "e4"  "f4"  "g4"  "h4" 
    "a5"  "b5"  "c5"  "d5"  "e5"  "f5"  "g5"  "h5" 
    "a6"  "b6"  "c6"  "d6"  "e6"  "f6"  "g6"  "h6" 
    "a7"  "b7"  "c7"  "d7"  "e7"  "f7"  "g7"  "h7" 
    "a8"  "b8"  "c8"  "d8"  "e8"  "f8"  "g8"  "h8"
))


(defvar reprSign '("" "P" "N" "B" "R" "Q" "K"))

(defvar cordDic '())

(let ((cord 0))
  (dolist (name reprCord)
    (setf cordDic (cons (cons name cord) cordDic))
    (incf cord)))


(defvar dir '(()
	      (9 11) 
	      (-21 -19 -12 -8 8 12 19 21)
	      (-11 -9 9 11)
	      (-10 -1 1 10)
	      (-11 -10 -9 -1 1 9 10 11)
	      (-11 -10 -9 -1 1 9 10 11)
	      (-9 -11)
	      (9 10 11)
	      (-9 -10 -11)))

(defvar moveArray (make-array '(10 64)))

(defvar epHashes (make-array '(64)))

(defvar sliders '(() () () T T T () () () ()))

(defvar pmap '(
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1  0  1  2  3  4  5  6  7 -1
    -1  8  9 10 11 12 13 14 15 -1
    -1 16 17 18 19 20 21 22 23 -1
    -1 24 25 26 27 28 29 30 31 -1
    -1 32 33 34 35 36 37 38 39 -1
    -1 40 41 42 43 44 45 46 47 -1
    -1 48 49 50 51 52 53 54 55 -1
    -1 56 57 58 59 60 61 62 63 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 
))

(defvar directions (make-array '(64 64) :initial-element -1))

(defvar rays (make-array '(64 8) :initial-element 0))

(defvar bitPosArray (make-array '(64)))      



(defvar index64 #(63 0 58 1 59 47 53 2
		  60 39 48 27 54 33 42 3
	          61 51 37 40 49 18 28 20
		  55 30 34 11 43 14 22 4
	          62 57 46 52 38 26 32 41
		  50 36 17 19 29 10 13 21
	          56 45 25 31 35 16 9 12
		  44 24 15  8 23 7 6 5))


(defun lastBit (bb) (let* ((debruijn64 #x07EDD5E59A4E28C2)
			   (position (mod (ash (* (logand bb (- bb)) debruijn64) -58) 64)))
		      (- 63 (aref index64 position))))

(defun firstBit (bb) 
 (let ((result 0)
       (bbtemp bb))
  (cond ((> bbtemp #xFFFFFFFF) 
	 (setf bbtemp (ash bbtemp -32) result (+ result 32)))
	((> bbtemp #xFFFF) 
	 (setf bbtemp (ash bbtemp -16) result (+ result 16)))
	((> bbtemp #xFF) 
	 (setf bbtemp (ash bbtemp -8) result (+ result 8))))
 (- 63 (+ result (floor (log bbtemp 2))))))


(loop for i below 64 do
      (setf (aref bitPosArray i) (expt 2 (- 63 i))))

(defun returnWithSetBit (b i)
  (logior b (aref bitPosArray i)))


(defun convertBitPos (n) 
  (if (= n 1) 0 
    (+ 1 (convertBitPos (/ n 2)))))

(defun flatten1 (lst)
  (if (null lst) '()
    (if (atom (car lst)) (cons (car lst) (flatten1 (cdr lst)))
      (append (flatten1 (car lst)) (flatten1 (cdr lst))))))

(defun flatten (x)
  (labels ((rec (x acc)
		(cond ((null x) acc)
		      ((atom x) (cons x acc))
		      (t (rec 
			  (car x)
			  (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro with-collector ((&optional (collector-name 'collect) 
				     (return-name 'ret)
				     (flat-name 'flat))
			  &body body)
  (let ((result (gensym))
	(retval (gensym))
	(flat-result (gensym)))
    `(let ((,result (list))
	   (,retval nil)
	   (,flat-result nil))
       (flet ((,collector-name (arg) (push arg ,result))
	      (,return-name (arg) (setf ,retval arg))
	      (,flat-name () (setf ,flat-result t)))
         (progn ,@body)
         (cond (,flat-result (flatten ,result))
	       (,result ,result)
	       (t ,retval))))))

(defmacro do-bits ((var x) &rest body)
  "Evaluates [body] forms after binding [var] to each set bit in [x]"
  (let ((k (gensym)))
    `(with-collector ()
       (do ((,k ,x (logand ,k (1- ,k))))
           ((= ,k 0))
         (let ((,var (- 63 (convertBitPos (logand ,k (- ,k))))))
           ,@body)))))


(defun createBoard (i) i)

(defmacro clearBit (b p)
  `(setf (ldb (byte 1 (- 63 ,p)) ,b) 0))


(defvar notBitPosArray (make-array '(64)))

(loop for i below 64 do
      (setf (aref notbitPosArray i) (lognot (expt 2 (- 63 i)))))

(defun returnWithClearBit (b i)
  (logand b (aref notBitPosArray i)))
  

(loop for piece from 1 to (- (length dir) 1) do
      (dotimes (fcord 120)
	(let ((g (nth fcord pmap)))
	     (if (not (= g -1)) 
		 (let ((b 0))
		   (dolist (d (nth piece dir))
		     (let* ((tcord fcord)
			    (p (nth tcord pmap)))
		       (loop
			(setf tcord (+ tcord d))
			(setf p (nth tcord pmap))
			(if (= p -1) (return))
			(setBit b p)
			(if (not (nth piece sliders))
			    (return)))))
		   (setf (aref moveArray piece g) b))))))

      
(loop for fcord from 0 to 119 do
      (let ((g (nth fcord pmap))
	    (ray -1))
	(if (not (= g -1))
	    (loop for piece in `(,BISHOP ,ROOK) do
		  (loop for d in (nth piece dir) do
			(setf ray (+ ray 1))
			(let ((a 0)
			      (tcord fcord))
			  (loop		      
			   (setf tcord (+ tcord d))
			   (setf a (nth tcord pmap))
			   (if (= a -1) (return))
			   (setBit (aref rays g ray) a)
			   (setf (aref directions g a) ray))))))))

(defvar fromToRay (make-array '(64 64)))

(loop for piece in `(,Bishop ,ROOK) do
      (loop for fcord from 0 to 119 do
	    (let ((g (nth fcord pmap)))
	      (if (not (= g -1))
		  (loop for d in (nth piece dir) do
			(let* ((tcord fcord)
			      (a (nth tcord pmap)))
			  (loop 
			   (let ((b (aref fromToRay g a)))
			     (setf tcord (+ tcord d))
			     (setf a (nth tcord pmap))
			     (if (= a -1) (return))
			     (setBit (aref fromToRay g a) a)
			     (setf (aref fromToRay g a)
				   (logior (aref fromToRay g a)
					   b))))))))))
(defvar B_OOOhash (expt 2 3))				 
(defvar B_OOhash (expt 2 2))
(defvar w_OOOhash (expt 2 1))
(defvar w_OOhash (expt 2 0))



(defvar B_OOO (expt 2 3))				 
(defvar B_OO (expt 2 2))
(defvar w_OOO (expt 2 1))
(defvar w_OO (expt 2 0))
(defvar W_CASTLED 1)
(defvar B_CASTLED 2)
(defconstant WHITE 0)

(defclass board ()
 ((blocker    :accessor blocker    :initarg :blocker    :initform  0)
  (friends    :accessor friends    :initarg :friends    :initform (make-array '(2)))
  (kings      :accessor kings      :initarg :kings      :initform (make-array '(2)))
  (boards     :accessor boards     :initarg :boards     :initform (make-array '(2 7) :initial-element 0))
  (b-enpassant  :accessor enpassant  :initarg :enpassant  :initform -1)
  (color      :accessor color      :initarg :color      :initform  WHITE)
  (castling   :accessor castling   :initarg :castling   :initform  (logior B_OOO B_OO W_OOO W_OO))
  (hasCastled :accessor hasCastled :initarg :hasCastled :initform  (make-array '(2) :initial-element nil))
  (fifty      :accessor fifty      :initarg :fifty      :initform 0)
  (checked    :accessor checked    :initarg :checked    :initform nil)
  (opchecked  :accessor opchecked  :initarg :opchecked  :initform nil)
  (arBoard    :accessor arBoard    :initarg :arBoard    :initform (make-array '(64))) 
  (hash       :accessor hash       :initarg :hash       :initform 0)
  (pawnhash   :accessor pawnhash   :initarg :pawnhash   :initform 0)
  (history    :accessor history    :initarg :history    :initform '())
  (ini-kings  :accessor ini-kings  :initarg :ini-kings  :initform `#(,E1 ,E8))
  (ini-rooks  :accessor ini-rooks  :initarg :ini-rooks  :initform `#(#(,A1 ,H1) #(,A8 ,H8)))))



(defun setbit-board (b color piece cord)
  (setBit (aref (boards b) color piece) cord))

(defun removebit-board (b color piece cord)
  (setBit (aref (boards b) color piece) cord))

(defun setpieceHash (b color piece cord) 
  (setf (hash b) (logand (hash b) (aref pieceHashes color piece cord))))

(defun setpawnhash (b color cord)
  (setf (hash b) (logior (hash b) (aref pieceHashes color PAWN cord))))

(defun set-arBoard (b cord piece)
  (setf (aref (arBoard b) cord) piece))

(defun set-kings (b color cord)
  (setf (aref (kings b) color) cord))


(defvar passedPawnMask (make-array '(2 64)))

(loop for cord below 64
      do 
      (setf (aref passedPawnMask WHITE (mod cord 64)) (aref rays cord 7))
      (setf (aref passedPawnMask WHITE (mod cord 64)) (aref rays cord 4))
      (if (not (= (logand cord 7) 0))
	  (progn
	    (setf (aref passedPawnMask WHITE cord) (aref rays (mod (- cord 1) 64) 7))
	    (setf (aref passedPawnMask WHITE cord) (aref rays (mod (- cord 1) 64) 4))))
      (if (not (= (logand cord 7) 0))
	  (progn
	    (setf (aref passedPawnMask WHITE cord) (aref rays (mod (+ cord 1) 64) 7))
	    (setf (aref passedPawnMask WHITE cord) (aref rays (mod (+ cord 1) 64) 4)))))


(defvar isolaniMask (make-array '(8)))

(setf (aref isolaniMask 0) (aref fileBits 1)) 
(setf (aref isolaniMask 7) (aref fileBits 6))
(loop for i from 1 to 7 
      do 
      (setf (aref isolaniMask 7) 
	    (logior (aref fileBits (- i 1)) (aref fileBits (mod (+ i 1) 8)))))


(defvar squarePawnMask (make-array '(2 64)))

(defun RANK (cord) (ash cord 3))
(defun FILE (cord) (logand cord 7))

(loop for cord below 64
      do
      (let* ((l (- 7 (RANK cord)))
	     (i (max (logand cord 56) (- cord l)))
	     (j (min (logior cord 7) (+ cord l))))
	(loop for k from i to (+ j 1) do
	      (setf (aref squarePawnMask WHITE cord)
		    (logior
		     (aref squarePawnMask WHITE cord)
      		     (aref bitPosArray k)
		     (aref fromToRay k (logior k 56))))))
      (let* ((l (RANK cord))
	     (i (max (logand cord 56) (- cord l)))
	     (j (min (logior cord 7) (+ cord l))))
	(loop for k from i to (+ j 1) do
	      (let ((kmod64 (mod k 64)))
	      (setf (aref squarePawnMask BLACK cord)
		    (logior
		     (aref squarePawnMask BLACK cord)
      		     (aref bitPosArray kmod64)
		     (aref fromToRay kmod64 (logand kmod64 7))))))))


(loop for cord from A2 to (+ H2 1) do
      (setf (aref squarePawnMask WHITE cord) 
	    (aref squarePawnMask WHITE (+ cord 8))))

(loop for cord from A7 to (+ H7 1) do
      (setf (aref squarePawnMask BLACK cord) 
	    (aref squarePawnMask BLACK (- cord 8))))

(defvar ray00 (make-array '(64)))
(defvar ray45 (make-array '(64)))
(defvar ray90 (make-array '(64)))
(defvar ray135 (make-array '(64)))

(loop for cord below 64 do
      (setf (aref ray00 cord) (logior (aref rays cord 5)
				      (aref rays cord 6)
				      (ash 1 (- 63 cord))))
      (setf (aref ray45 cord) (logior (aref rays cord 0)
				      (aref rays cord 3)
				      (ash 1 (- 63 cord))))
      (setf (aref ray90 cord) (logior (aref rays cord 4)
				      (aref rays cord 7)
				      (ash 1 (- 63 cord))))
      (setf (aref ray135 cord) (logior (aref rays cord 1)
				      (aref rays cord 2)
				      (ash 1 (- 63 cord)))))

(defmacro setAttack (attack cord map item) 
  `(setf (gethash ,map (aref ,attack ,cord)) ,item))

(defun getAttack (attack cord map)
  (gethash map (aref attack cord)))

(defvar attack00 (map-into (make-array '64) 'make-hash-table))
(defvar attack45 (map-into (make-array '64) 'make-hash-table))
(defvar attack90 (map-into (make-array '64) 'make-hash-table))
(defvar attack135 (map-into (make-array '64) 'make-hash-table))

(defvar cmap '(128 64 32 16 8 4 2 1))
(defvar rot1 `#(,A1 ,A2 ,A3 ,A4 ,A5 ,A6 ,A7 ,A8))
(defvar rot2 `#(,A1 ,B2 ,C3 ,D4 ,E5 ,F6 ,G7 ,H8))
(defvar rot3 `#(,A8 ,B7 ,C6 ,D5 ,E4 ,F3 ,G2 ,H1))
