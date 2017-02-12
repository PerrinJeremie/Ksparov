object Aux{
  def on_board (x : Int, y : Int) = {
	(0<=x)&&(x<=7)&&(0<=y)&&(y<=7);}
  def checks_pre_move(x : Int, x_a : Int, y : Int, y_a : Int, f : (Int,Int)=>(Boolean))=
    {!(x_a==x && y_a==y) && on_board(x_a, y_a) && f(x_a,y_a)}
  def get_dirs (x_d : Int ,x_a : Int, y_d : Int, y_a : Int) : (Int,Int)=
     {(math.signum(x_a-x_d),math.signum(y_a-y_d))}
  var cases = new Array[(Int,Int)](64)
  for (i<-0 to 7){for(j<-0 to 7) {cases(8*i+j)=(i,j)}}
  }

abstract class Piece (play : Int, x : Int, y : Int) {
  var pos_x = x
  var pos_y = y
  def coords = (pos_x,pos_y)
  var name : String
  var player = play
  def pattern(x_a : Int, y_a : Int) : Boolean
  def piece_of_coord (x : Int, y : Int, g : Array[Piece]) : Option[Piece] = {
    g find (p => p.pos_x==x && p.pos_y ==y) }

  /*Recursive function that explores the path of a movement*/
  def clear_path (x_d : Int, y_d : Int, x_a : Int, y_a : Int,g : Array[Piece], dir_x : Int, dir_y : Int) :( Boolean,Option[Piece])={
    //Getting next case
    var next_x = dir_x+x_d
    var next_y = dir_y+y_d
    var p = piece_of_coord (next_x,next_y,g)
    //Base case
    if (x_a==next_x && y_a==next_y){ p match {
       case None => (true,None) //arrival is free
       case _ if (p.get).player == player => (false,Some(p.get)) //Friendly piece on arrival
       case _ if (p.get).player != player => (true, Some(p.get)) //Ennemy piece on arrival
      }
    }
    //Recursive case
    else  {p match {
       case None => clear_path(next_x,next_y,x_a,y_a,g,dir_x,dir_y)
       case _ => (false,Some(p.get)) //There is an obstacle on the path
    }
   }
  }

  /*Check for attacks on both kings and returns a list of attackers for each king.*/
  def check_check (g:Array[Piece]):Array[List[Piece]]={
    var attacks = Array(Nil:List[Piece],Nil:List[Piece])

    //finding the kings
    var white_king = (g find (p=> p.name=="king" && p.player==1)).get
    var black_king = (g find (p=> p.name=="king" && p.player==0)).get
    var (x_wk,y_wk) = white_king.coords
    var (x_bk,y_bk) = black_king.coords
    var kings = Array(x_wk, y_wk, x_bk, y_bk)

    //Now to check if any piece is attacking a king
    for (i<-0 to g.size-1){
      var p = g(i)
      var (x,y,color) = (p.pos_x, p.pos_y, p.player)
      //Checking if the piece isn't dead :
      if (Aux.on_board(x,y)){
        var x_king = kings(2*color)
        var y_king = kings(2*color+1)
        //Checking the validity of the move :
        if (Aux.checks_pre_move(x, x_king, y, y_king, p.pattern)){
        var (dir_x,dir_y)= Aux.get_dirs(x, x_king, y, y_king)
        //Checking if the piece can actually attack the king :
        var (clear,piece_on_arrival) = p.clear_path(x, y, x_king, y_king, g, dir_x, dir_y)
        if (clear) {attacks(1-color) = attacks(1-color):+p}
       }
     }
   }
  attacks}

  /*Auxiliary function for move : checks if a move is possible but doesn't modify the board*/
  /*Returns the validity of the move, the piece taken, and a list of opponent's king attackers*/
  def pre_move(x_a : Int, y_a : Int, g :Array[Piece]) : (Boolean, Option[Piece], List[Piece])={

  //Checking if the arrival or the piece is on board and if the pattern is respected
	if (!(Aux.checks_pre_move(pos_x,x_a, pos_y, y_a, pattern))) {
    (false,Some(this),Nil)}
    //Checking the path and the destination
    else {
    var (dir_x, dir_y) = Aux.get_dirs(pos_x,x_a,pos_y,y_a)
    var (clear, p_arrival) = clear_path(pos_x,pos_y,x_a,y_a,g,dir_x,dir_y)
    if (!clear){
      (false, p_arrival, Nil)} //There is a friendly piece on the destination
      else {
      //Saving current piece coordinates
      var (old_x,old_y) = coords
      var (old_x2,old_y2)=(0,0)
      pos_x = x_a
      pos_y = y_a
      p_arrival match {
        case None => ();
        case _ => old_x2  =  p_arrival.get.pos_x ;
                  old_y2 =  p_arrival.get.pos_y ;
                  p_arrival.get.pos_x=(-1);p_arrival.get.pos_y=(-1)
       }
    //Checking if the king is attacked
    val checks = check_check(g)
    //Restablishing previous position
    pos_x = old_x
    pos_y = old_y
    if (p_arrival != None) {
    p_arrival.get.pos_x = old_x2 ; p_arrival.get.pos_y= old_y2 }
    if (!(checks(player).isEmpty)){
      (false, Some (checks(player)(0) ), checks(player)) //The king is attacked and this move doesn't protect the king
     }
     else {
     (true,p_arrival,checks(1-player)) } //This move is valid
    }
	 }
 }

 /*Returns the validity of movement and the piece taken or the reason why the move is invalid. If the move is valid, applies it.*/
 def move(x_a : Int, y_a : Int, g : Array[Piece]) : (Boolean, Option[Piece])={
   var (move_ok, p_arrival, attackers) = pre_move(x_a, y_a, g)
   if (move_ok){
     pos_x = x_a ;
     pos_y = y_a ;
     if (p_arrival != None) {
     p_arrival.get.pos_x = (-1) ; p_arrival.get.pos_y= (-1) }
     var king = ( g find ( p => (p.name == "king" && p.player==(1-player)))).get
     king.asInstanceOf[King].attackers = attackers
   }
   (move_ok, p_arrival)
  }

 /* Auxiliary function for possible_moves */
 def aux( x : (Int,Int), g : Array[Piece])={
   var (i,j) : (Int,Int) = x
   var (clear, a , b) = pre_move (i, j, g)
   clear
  }
/*Returns an array of all the cases the piece can go to */
 def possible_moves (g:Array[Piece])={
   Aux.cases.filter( aux(_,g) )
  }
 }

class Pawn (b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0) {
  var name = "pawn"
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
       case _ =>(false,Some(p.get))}
     }
    else {p match{
      case None =>(false,None)
      case _ =>(p.get.player != player, Some(p.get))
          }
        }
      }
}

class Rook(b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0) {
  var name = "rook"
  def pattern (x_a:Int,y_a:Int)= {
	(x_a == pos_x) || (y_a == pos_y)
  }
}

class Knight(b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0) {
  var name = "knight"
  var has_moved = false;
  def pattern (x_a:Int,y_a:Int)={//A knight can reach a case if it is at distance 3 and not aligned with its position
	math.abs(pos_x-x_a) + math.abs(pos_y-y_a)==3 && (pos_x!=x_a) && (pos_y!=y_a)}

  /*Knighs can jump over all other pieces*/
  override def clear_path(x_d:Int,y_d:Int,x_a:Int,y_a:Int,g:Array[Piece],dir_x:Int,dir_y:Int):(Boolean,Option[Piece])={
    var p=piece_of_coord(x_a,y_a,g)
    p match {
      case None => (true,None)
      case _ if (p.get).player == player => (false,Some(p.get))
      case _ if (p.get).player != player => (true, Some(p.get))
    }
   }
  }

class Bishop(b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0){
  var name = "bishop"
  def pattern(x_a:Int,y_a:Int)={
	(pos_x-x_a)==(pos_y-y_a) || (pos_x-x_a)== (y_a-pos_y)
  }
}

class King (b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0) {
  var name = "king"
  var attackers:List[Piece]=Nil
  var has_moved:Boolean = false
  def attacked = !attackers.isEmpty
  def pattern(x_a : Int, y_a : Int)={
	(math.abs(pos_x-x_a)<=1) && (math.abs(pos_y-y_a)<=1)}
}

class Queen (b : Int, x0 : Int, y0 : Int) extends Piece (b, x0, y0) {
  var name = "queen"
  def pattern(x_a:Int,y_a:Int)={
	(x_a == pos_x) || (y_a == pos_y) || (pos_x-x_a)==(pos_y-y_a) || (pos_x-x_a)== (y_a-pos_y)
  }
}

object General {
  def move_is_possible(p : Piece, x_a : Int, y_a : Int, g :Array[Piece] ) : Boolean ={
    var (possible, a, b) = p.pre_move(x_a, y_a, g)
    possible
  }
  /*Checks if player pl has lost the game. */
  def check_mate(g : Array[Piece], pl : Int) = {
    var king = (g find ( p => (p.name == "king" && p.player==pl))).get
    var (x_king, y_king) = king.coords
    var checkmate = true
    for (i<-(-1) to 1 ; j<-(-1) to 1){
        checkmate = checkmate && !(move_is_possible (king, x_king+i, y_king+j, g))
      }
    if (checkmate && (king.asInstanceOf[King]).attackers.size == 1) {
        var (x_att,y_att) = (king.asInstanceOf[King]).attackers(0).coords
        for (p<-g if p.player == pl){
          var (x,y) = (x_att, y_att)
          var (dir_x,dir_y) = Aux.get_dirs(x_att, x_king, y_att, y_king)
          while (checkmate && ! (x == x_king && y == y_king)) {
          checkmate = checkmate & !move_is_possible(p, x, y, g)
          x += dir_x
          y += dir_y
        }
    }
  }
  checkmate
 }
}

/*object Program{
  def main(args:Array[String]):Unit={
  var black_king = new King(0,0,0)
  var white_king = new King(1,7,7)
  var black_rook1 = new Rook (0,1,1)
  var black_bishop = new Bishop(0,1,0)
  var white_rook = new Rook (1,1,4)
  var g = Array(black_king, black_rook1, black_bishop, white_king, white_rook)
  white_rook.move(0,4,g)
  println( General.check_mate(g, 0) )
  }
}*/
