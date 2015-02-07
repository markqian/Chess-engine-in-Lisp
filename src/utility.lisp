(load "~/chess/src/data.lisp")

(defun combine-element (f v c n)
  (if (= n 0) (funcall f (aref v c 0))
    (funcall f (aref v c n) (combine-element f v c (- n 1)))))

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
  
(defun getAttack (attack cord map)
  (gethash map (aref attack cord)))

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

(defmacro clearBit (b p)
  `(setf (ldb (byte 1 (- 63 ,p)) ,b) 0))

(defun returnWithClearBit (b i)
  (logand b (aref notBitPosArray i)))

(defun returnWithSetBit (b i)
  (logior b (aref bitPosArray i)))

(defun getfrom (move) (logand (ash move -6) 63))

(defun getto (move) (logand move 63))

(defun getflag (move) (ash move -12))

(defun flatten1 (lst)
  (if (null lst) '()
    (if (atom (car lst)) (cons (car lst) (flatten1 (cdr lst)))
      (append (flatten1 (car lst)) (flatten1 (cdr lst))))))


(defun createBoard (i) i)

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

(defun reduce1 (f lst)
  (if (null lst) lst
    (funcall f (car lst) (reduce f (cdr lst)))))

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

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

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun tokenize-string (str)
  (my-split str))

(defun zero (i)
  (= i 0))

(defun not-zero (i)
  (not (zero i)))

(defun getEnemies (b c)
  (aref (friends b) (- 1 c)))

(defun notfriends (b) (lognot (aref (friends b) (color b))))

(defun pboards (b piece color)
  (aref (boards b) color piece))

(defun move-array (piece cord)
  (aref moveArray piece cord))

(defun pieces (b color piece)
  (aref (boards b) color piece))
