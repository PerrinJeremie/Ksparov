object Aux{
  def on_board(x:Int,y:Int):Boolean={
	(0<=x)&&(x<=7)&&(0<=y)&&(y<=7)}
  }

abstract class Piece (name : String, play : Int, x:Int,y:Int) {
  var pos_x = x
  var pos_y = y
  var piece_name = name
  var player = play
  def pattern(x_a:Int,y_a:Int):Boolean
  def piece_of_coord (x:Int,y:Int,g:Array[Piece]):Option[Piece]={
    var res :Option[Piece] = None
    for (p<-g){
      if (p.pos_x==x && p.pos_y==y){
        res = Some(p)
      }}
    res}
  def clear_path (x_d:Int,y_d:Int,x_a:Int,y_a:Int,g:Array[Piece],dir_x:Int,dir_y:Int):(Boolean,Option[Piece])={
    var next_x=dir_x+x_d
    /*if (x_a!=x_d){
      next_x = dir_x*math.abs(x_a-x_d)/(x_a-x_d)+x_d}*/
    var next_y=dir_y+y_d
    /*if (y_a!=y_d){
      next_y = dir_y*math.abs(y_a-y_d)/(y_a-y_d)+y_d}*/
    var p:Option[Piece]=piece_of_coord(next_x,next_y,g)
    if (x_a==next_x && y_a==next_y){p match {
       case None => (true,None)
       case _ if (p.get).player==this.player => (false,Some(p.get))
       case _ if (p.get).player != this.player => (true, Some(p.get))
      }
    }
    else {
    p match {
       case None => clear_path(next_x,next_y,x_a,y_a,g,dir_x,dir_y)
       case _ => println(p.get.piece_name) ; (false,Some(p.get))
    }}
  }
  def move(x_a:Int,y_a:Int,g:Array[Piece]):Unit={
	if (!(x_a==pos_x && y_a==pos_y) && Aux.on_board(x_a,y_a) && pattern(x_a,y_a)) {
    var dir_x=0
    var dir_y=0
    if (x_a!=pos_x){
    dir_x = (x_a-pos_x)/math.abs(pos_x-x_a)}
    if (y_a!=pos_y) {
    dir_y = (y_a-pos_y)/math.abs(pos_y-y_a)}
    var (clear,piece_on_arrival)=clear_path(pos_x,pos_y,x_a,y_a,g,dir_x,dir_y)
    if (clear){
      pos_x = x_a
      pos_y = y_a
      piece_on_arrival match {
        case None => ();
        case _ => piece_on_arrival.get.pos_x=(-1);piece_on_arrival.get.pos_y=(-1)
      }
    }
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
  var has_moved = false;
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
  var has_moved:Boolean = false
  def pattern(x_a:Int,y_a:Int)={
	(math.abs(pos_x-x_a)<=1) && (math.abs(pos_y-y_a)<=1)}
}

class Queen (name:String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern(x_a:Int,y_a:Int)={
	(x_a == pos_x) || (y_a == pos_y) || (pos_x-x_a)==(pos_y-y_a) || (pos_x-x_a)== (y_a-pos_y)
  }
}


/*object Program{
  def main(args:Array[String]):Unit={
	var fou  = new Bishop("FouBlanc",1,0,0)
  var fou2 = new Bishop("FouNoir",0,4,4)
  }
}*/
