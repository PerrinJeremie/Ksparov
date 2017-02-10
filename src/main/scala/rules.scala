object Aux{
  def on_board (x:Int,y:Int) = {
	(0<=x)&&(x<=7)&&(0<=y)&&(y<=7);}
  def checks_pre_move(x_a:Int,x:Int,y_a:Int,y:Int,f:(Int,Int)=>(Boolean))={!(x_a==x && y_a==y) && on_board(x_a,y_a) && f(x_a,y_a)}
  def get_dirs (x_d:Int,x_a:Int,y_d:Int,y_a:Int):(Int,Int)= {(math.signum(x_a-x_d),math.signum(y_a-y_d))}}

abstract class Piece (name : String, play : Int, x:Int,y:Int) {
  var pos_x = x
  var pos_y = y
  var coords = (x,y)
  var piece_name = name
  var player = play
  def pattern(x_a:Int,y_a:Int):Boolean
  def piece_of_coord (x:Int,y:Int,g:Array[Piece]):Option[Piece]={
    g find (p => p.pos_x==x && p.pos_y ==y) }

  /*Recursive function that explores the path of a movement*/
  def clear_path (x_d:Int,y_d:Int,x_a:Int,y_a:Int,g:Array[Piece],dir_x:Int,dir_y:Int):(Boolean,Option[Piece])={
    var next_x=dir_x+x_d
    var next_y=dir_y+y_d
    var p=piece_of_coord(next_x,next_y,g)
    if (x_a==next_x && y_a==next_y){ p match { //base case
       case None => (true,None) //arrival is free
       case _ if (p.get).player==this.player => (false,Some(p.get)) //friendly piece on arrival
       case _ if (p.get).player != this.player => (true, Some(p.get)) //ennemy piece on arrival
      }
    }
    else {
    p match { //recursive case
       case None => clear_path(next_x,next_y,x_a,y_a,g,dir_x,dir_y)
       case _ => (false,Some(p.get)) //there is an obstacle on the path
    }}
  }

  /*Check for attacks on both kings*/
  def check_check (g:Array[Piece]):Array[Boolean]={
    var attacks = Array(false,false)
    var white_king = (g find (p=> p.piece_name=="king" && p.player==1)).get
    var black_king = (g find (p=> p.piece_name=="king" && p.player==0)).get
    var (x_wk,y_wk) = white_king.coords
    var (x_bk,y_bk) = black_king.coords
    var kings = Array(x_wk,y_wk,x_bk,y_bk)
    for (i<-0 to g.size-1){
      var p=g(i)
      var (x,y,color)=(p.pos_x,p.pos_y,p.player)
      if (Aux.on_board(x,y)){
        var x_king = kings(2*color)
        var y_king = kings(2*color+1)
        if (Aux.checks_pre_move(x,x_king,y,y_king,p.pattern)){
        var (dir_x,dir_y)= Aux.get_dirs(pos_x,x_king,pos_y,y_king)
        var (clear,piece_on_arrival)=clear_path(x,y,x_king,y_king,g,dir_x,dir_y)
        if (clear){attacks(1-color)=true}}
    }}
    attacks }

/*Return the validity of movement, the piece taken or the reason why the move is invalid, and if the king is attacked*/
  def move(x_a:Int,y_a:Int,g:Array[Piece]):(Boolean,Option[Piece],Boolean)={
	if (!(Aux.checks_pre_move(x_a,pos_x,y_a,pos_y,pattern))) {
    (false,Some(this),false)}//The arrival or the piece is not on board, or the pattern is'nt respected
    else{
    var (dir_x,dir_y)=Aux.get_dirs(pos_x,x_a,pos_y,y_a)
    var (clear,p_arrival)= clear_path(pos_x,pos_y,x_a,y_a,g,dir_x,dir_y) //checking the path and the destination
    if (!clear){
      (clear,p_arrival,false)} //There is a friendly piece on the destination
      else {
      var (old_x,old_y)=(pos_x,pos_y)
      var (old_x2,old_y2)=(0,0)
      pos_x = x_a
      pos_y = y_a
      p_arrival match {
        case None => ();
        case _ => var (old_x2,old_y2):(Int,Int) = (p_arrival.get.pos_x,p_arrival.get.pos_y);
                  p_arrival.get.pos_x=(-1);p_arrival.get.pos_y=(-1)
      }
    val checks=check_check(g) //checking if the king is attacked
    val pl=player
     if (checks(pl)){
       var (pos_x,pos_y):(Int,Int)=(old_x,old_y)
       if (p_arrival!=None){ p_arrival.get.pos_x = old_x2 ; p_arrival.get.pos_y=old_y2 }
       var king= (g find (p=> p.piece_name=="king" && p.player==player)).get
       (false,king,false) //The king is attacked and this move doesn't protect the king
     }
     (true,p_arrival,checks(1-pl)) //This move is valid
    }
	 }
 }
}

class Pawn (name :String,b:Int,x0:Int,y0:Int) extends Piece (name,b,x0,y0) {
  def pattern(x_a:Int,y_a:Int)={
	if (player==1) {
	  pos_x==x_a && (y_a==pos_y+1 || y_a==pos_y+2 && y_a==1) || ( (y_a==pos_y+1) && (math.abs(pos_x-x_a)==1) )  ;}
	else {
	  pos_x==x_a && (y_a==pos_y-1 || y_a==pos_y-2 && y_a==6) || ( (y_a==pos_y-1) && (math.abs(pos_x-x_a)==1) )  ;}
  }
  /*Pawns can only attack diagonally, and not while going forward*/
  override def clear_path (x_d:Int,y_d:Int,x_a:Int,y_a:Int,g:Array[Piece],dir_x:Int,dir_y:Int):(Boolean,Option[Piece])={
    var p:Option[Piece]=piece_of_coord(x_a,y_a,g)
    if (x_a==pos_x) {p match {
       case None => (true,None)
       case _ =>(false,Some(p.get))}}
    else {p match{
      case None =>(false,None)
      case _ =>( p.get.player!=player,Some(p.get))
          }
        }
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
	math.abs(pos_x-x_a) + math.abs(pos_y-y_a)==3 && (pos_x!=x_a) && (pos_y!=y_a)}//A knight can reach a case if it is at distance 3 and not aligned with its position
  /*Knighs can jump over all other pieces*/
  override def clear_path(x_d:Int,y_d:Int,x_a:Int,y_a:Int,g:Array[Piece],dir_x:Int,dir_y:Int):(Boolean,Option[Piece])={
    var p=piece_of_coord(x_a,y_a,g)
    p match {
      case None => (true,None)
      case _ if (p.get).player==this.player => (false,Some(p.get))
      case _ if (p.get).player != this.player => (true, Some(p.get))
    }
  }
  }

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

object Program{
  def check_mate(g:Array[Piece],pl:Int)={}
  def main(args:Array[String]):Unit={
  var black_king = new King("king",0,0,0)
  var white_king = new King("king",1,7,7)
  var white_bishop = new Bishop ("bishop",1,3,3)
  var black_rook = new Rook("rook",0,4,3)
  var g=Array(white_bishop,black_rook,black_king,white_king)
  black_rook.move(3,3,g)
  }
}
