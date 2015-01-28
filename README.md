**Introduction**

This is a chess engine written in Common Lisp. It uses bitboards for parallel move generation and a very basic eval function to evaluate positions. The alphaBeta search function is based off the one found on [Chess Programming](http://chessprogramming.wikispaces.com/Alpha-Beta).

**To Make/Install**

There is no need to make the program. You just have to install xboard and sbcl. I have not tested this program on any other platform other than Linux. 

To install xboard and sbcl just type:

sudo apt-get install xboard sbcl

**To run**

./run

**Screenshots**

![Alt text](https://github.com/markqian/Chess-engine-in-Lisp/blob/master/screenshots/screenshot.png)

**Video Demo**

[![Screenshot](https://github.com/markqian/Chess-engine-in-Lisp/blob/master/screenshots/screenshot2.png)](http://youtu.be/SzdItkudGLw)

As you can see the eval function is very primitive. The engine some time makes move that made little sense. The engine should improve a lot more when I implement more advance algorithms.










