# Ksparov
Repository for Ksparov, our awesome chess game.

You can find the Doc at : siolan.ddns.net/Ksparov

(*************** Useful Information *************)

-Rows and Columns are in [0,7]
-the zero of the coordinate system is on the bottom left corner, x axis horizontal, y axis vertical.
-A dead piece has coordinates x=-1,y=-1
-A game is an Array of (32) Pieces structured in this way:
   (The piece with the lowest x-value always comes first)
   -The first 8 pieces are the white pawns 
   -Then the 2 white rooks
   -Then the 2 white knights
   -Then the 2 bishops
   -Then the queen, then the king
   And then the same for the black pieces 
