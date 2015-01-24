(defstruct (move (:conc-name m)) (moveInt 0))

(defmacro clear (move)
  `(setf (mmoveInt ,move) 0))
 
(defmacro setFrom (from move)
  `(setf (mmoveInt ,move) (logand (mmoveInt ,move) #xffffffc0))
  `(setf (mmoveInt ,move) (logxor (mmoveInt ,move) (logand ,from #x0000003f))))

(defmacro setTosq (tosq move)
  `(setf (mmoveInt ,move) (logand (mmoveInt ,move) #xfffff03f))
  `(setf (mmoveInt ,move) (logxor (mmoveInt ,move) (ash (logand ,tosq #x0000003f) -6))))
  
(defmacro setPiec (piec move)
  `(setf (mmoveInt ,move) (logand (mmoveInt ,move) #xffff0fff))
  `(setf (mmoveInt ,move) (logxor (mmoveInt ,move) (ash (logand ,piec #x0000000f) -12))))
 
(defmacro setCapt (cap move)
  `(setf (mmoveInt ,move) (logand (mmoveInt ,move) #xfff0ffff))
  `(setf (mmoveInt ,move) (logxor (mmoveInt ,move) (ash (logand ,cap #x0000000f) -16))))
  
(defmacro setProm (prom move)
  `(setf (mmoveInt ,move) (logand (mmoveInt ,move) #xff0fffff))
  `(setf (mmoveInt ,move) (logxor (mmoveInt ,move) (ash (logand ,prom #x0000003f) -20))))
        
(defun getFrom (move)
  (logand (mmoveInt move) #x0000003f)) 

(defun getTosq (move)
 (logand (ash (mmoveInt move) 6) #x0000003f))
 
(defun getPiec (move)
  (logand (ash (mmoveInt move) 12) #x0000000f))  
 
(defun getCapt (move)
  (logand (ash (mmoveInt move) 16) #x0000000f))   
 
(defun getProm (move)
  (logand (ash (mmoveInt move) 20) #x0000000f))

(defun isWhitemove (move)
  (= (logand (lognot (mmoveInt move)) #x00008000) #x00008000))
 
(defun isBlackmove (move)
  (= (logand (mmoveInt move) #x00008000) #x00008000))
 
(defun isCapture (move)
  (not (= (logand (mmoveInt move) #x000f0000) #x00000000)))

(defun isKingcaptured (move)
  (= (logand (mmoveInt move) #x00070000) #x00020000))
 
(defun isRookmove (move)
  (= (logand (mmoveInt move) #x00007000) #x00006000))
  
(defun isRookcaptured (move)
  (= (logand (mmoveInt move) #x00070000) #x00060000))

(defun isKingmove (move)
  (= (logand (mmoveInt move) #x00007000) #x00002000))) 

(defun isPawnmove (move)
  (= (logand (mmoveInt move) #x00007000) #x00001000)) 
  
(defun isPawnDoublemove (move)
  (or (and (= (logand (mmoveInt move) #x00007000) #x00001000) 
	   (= (logand (mmoveInt move) #x00000038) #x00000008)
	   (= (logand (mmoveInt move) #x00000e00) #x00000600))
      (and (= (logand (mmoveInt move) #x00000038) #x00000030) 
	   (= (logand (mmoveInt move) #x00000e00) #x00000800))))
 
(defun isEnpassant (move)
  (= (logand (mmoveInt move) #x00700000) #x00100000))

(defun isPromotion (move)
  (> (logand (mmoveInt move) #x00700000) #x00200000))

(defun isCastle (move)
  (= (logand (mmoveInt move) #x00700000) #x00200000))

(defun isCastleOO (move)
  (= (logand (mmoveInt move) #x007001c0) #x00200180))

(defun isCastleOOO (move)
  (= (logand (mmoveInt move) #x007001c0) #x00200080))
 
