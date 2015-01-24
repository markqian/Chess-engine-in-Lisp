inifile
pathname
cmd_buff
cmd_buff_count

(defparameter board (make-board))

(defconst A1 1)
(defconst B1 2)
(defconst C1 3)
(defconst D1 4)
(defconst E1 5)
(defconst F1 6)
(defconst G1 7)
(defconst H1 8)

(defconst A2 9)
(defconst B2 10)
(defconst C2 11)
(defconst D2 12)
(defconst E2 13)
(defconst F2 14)
(defconst G2 15)
(defconst H2 16)

(defconst A3 17)
(defconst B3 18)
(defconst C3 19)
(defconst D3 20)
(defconst E3 21)
(defconst F3 22)
(defconst G3 23)
(defconst H3 24)

(defconst A4 25)
(defconst B4 26)
(defconst C4 27)
(defconst D4 28)
(defconst E4 29)
(defconst F4 30)
(defconst G4 31)
(defconst H4 32)

(defconst A5 33)
(defconst B5 34)
(defconst C5 35)
(defconst D5 36)
(defconst E5 37)
(defconst F5 38)
(defconst G5 39)
(defconst H5 40)

(defconst A6 41)
(defconst B6 42)
(defconst C6 43)
(defconst D6 44)
(defconst E6 45)
(defconst F6 46)
(defconst G6 47)
(defconst H6 48)

(defconst A7 49)
(defconst B7 50)
(defconst C7 51)
(defconst D7 52)
(defconst E7 53)
(defconst F7 54)
(defconst G7 55)
(defconst H7 56)

(defconst A8 57)
(defconst B8 58)
(defconst C8 59)
(defconst D8 60)
(defconst E8 61)
(defconst F8 62)
(defconst G8 63)
(defconst H8 64)

(defconst square-name '("a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1",
			"a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2",
			"a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3",
			"a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4",
			"a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5",
			"a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6",
			"a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7",
			"a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"))

(defconst files '(1 2 3 4 5 6 7 8
		  1 2 3 4 5 6 7 8
		  1 2 3 4 5 6 7 8
	          1 2 3 4 5 6 7 8
		  1 2 3 4 5 6 7 8
	          1 2 3 4 5 6 7 8
		  1 2 3 4 5 6 7 8
	          1 2 3 4 5 6 7 8))

(defconst ranks '(1 1 1 1 1 1 1 1
		  2 2 2 2 2 2 2 2
                  3 3 3 3 3 3 3 3
                  4 4 4 4 4 4 4 4
                  5 5 5 5 5 5 5 5
                  6 6 6 6 6 6 6 6
                  7 7 7 7 7 7 7 7
                  8 8 8 8 8 8 8 8))

(defconst white-move 0)
(defconst black-move 1)

(defconst empty 0)
(defconst white-pawn 1)
(defconst white-king 2)
(defconst white-knight 3)
(defconst white-bishop 5)
(defconst white-rook 6)
(defconst white-queen 7)
(defconst black-pawn 9)
(defconst black-king 10)
(defconst black-knight 11)
(defconst black-rook 14)
(defconst black-queen 15)


(defconst piece-names '("  " "P " "K " "N " "  " "B " "R " "Q "
			 "  " "P*" "K*" "N*" "  " "B*" "R*" "Q*"))

(defconst piece-names '(" " " " "K" "N" " " "B" "R" "Q" " " " " "K" "N" " " "B" "R" "Q"))

