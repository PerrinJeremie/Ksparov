object Aux{
	def on_board(x:Int,y:Int):Boolean=
	(0<=x)&&(x<=7)&&(0<=y)&&(y<=7)}

abstract class Piece (x0:Int,y0:Int,b:Boolean) { //1 is for White, 0 for Black
	 var x = x0
   var y = y0
   var color = b
	 def pattern(xa:Int,ya:Int):Boolean
	 def move(x_a:Int,y_a:Int):Unit={
		 if (!(x_a==x && y_a==y) && Aux.on_board(x_a,y_a) && pattern(x_a,y_a)) {
			 x=x_a
			 y=y_a
		 }
	 }
 	 }
class Pawn (x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b) {
	def pattern(x_a:Int,y_a:Int)={
		if (color) {
			 x==x_a && (y_a==y+1 || y_a==y+2 && y_a==1) ;}
		else {
			x==x_a && (y_a==y-1 || y_a==y-2 && y_a==6) ;}
		}
	}
class Rook(x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b) {
	  def pattern (x_a:Int,y_a:Int)= {
	 		(x_a == x) || (y_a == y)
	 	 }
	 }
class Knight(x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b) {
	def pattern (x_a:Int,y_a:Int)={
		math.abs(x-x_a) + math.abs(y-y_a)==3 && (x!=x_a) && (y!=y_a)
		//A knight can reach a case if it is at distance 3 and not aligned with its position
	}}

class Bishop(x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b){
	def pattern(x_a:Int,y_a:Int)={
		(x-x_a)==(y-y_a) || (x-x_a)== (y_a-y)
	}}

class King(x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b) {
	var attacked:Boolean = false
	def pattern(x_a:Int,y_a:Int)={
		(math.abs(x-x_a)<=1) && (math.abs(y-y_a)<=1)}
	}

class Queen (x0:Int,y0:Int,b:Boolean) extends Piece (x0,y0,b) {
	def pattern(x_a:Int,y_a:Int)={
		//Bishop.pattern(x_a,y_a) || Rook.pattern(x_a,y_a)
		(x_a == x) || (y_a == y) || (x-x_a)==(y-y_a) || (x-x_a)== (y_a-y)
	}
}


object Program{
	def main(args:Array[String]):Unit={
	 var fou = new Bishop(1,0,true)
	 fou.move(5,4)
	 println(fou.x)
	 println(fou.y)
	}
	}
