object Aux{
  def on_board(x:Int,y:Int):Boolean={
	(0<=x)&&(x<=7)&&(0<=y)&&(y<=7)}}

abstract class Piece (name : String, play : Int, x:Int,y:Int) {
  var pos_x = x
  var pos_y = y
  var piece_name = name
  var player = play
  def pattern(xa:Int,ya:Int):Boolean
  def move(x_a:Int,y_a:Int):Unit={
	if (!(x_a==pos_x && y_a==pos_y) && Aux.on_board(x_a,y_a) && pattern(x_a,y_a)) {
	  pos_x=x_a
	  pos_y=y_a
	}
  }
}
class Pawn (name :String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern(x_a:Int,y_a:Int)={
	if (player==1) {
	  pos_x==x_a && (y_a==pos_y+1 || y_a==pos_y+2 && y_a==1) ;}
	else {
	  pos_x==x_a && (y_a==pos_y-1 || y_a==pos_y-2 && y_a==6) ;}
  }
}
class Rook(name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern (x_a:Int,y_a:Int)= {
	(x_a == pos_x) || (y_a == pos_y)
  }
}
class Knight(name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern (x_a:Int,y_a:Int)={
	math.abs(pos_x-x_a) + math.abs(pos_y-y_a)==3 && (pos_x!=x_a) && (pos_y!=y_a)
	//A knight can reach a case if it is at distance 3 and not aligned with its position
  }}

class Bishop(name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0){
  def pattern(x_a:Int,y_a:Int)={
	(pos_x-x_a)==(pos_y-y_a) || (pos_x-x_a)== (y_a-pos_y)
  }}

class King(name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  var attacked:Boolean = false
  def pattern(x_a:Int,y_a:Int)={
	(math.abs(pos_x-x_a)<=1) && (math.abs(pos_y-y_a)<=1)}
}

class Queen (name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern(x_a:Int,y_a:Int)={
	//Bishop.pattern(x_a,y_a) || Rook.pattern(x_a,y_a)
	(x_a == pos_x) || (y_a == pos_y) || (pos_x-x_a)==(pos_y-y_a) || (pos_x-x_a)== (y_a-pos_y)
  }
}


/*object Program{
  def main(args:Array[String]):Unit={
	var fou = new Bishop(1,0,true)
	fou.move(5,4)
	println(fou.x)
	println(fou.y)
  }
}*/
