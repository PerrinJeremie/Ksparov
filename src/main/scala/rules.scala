/** Auxiliary and general methods. Most are self-explainatory. */
object Aux {
  var cases = new Array [(Int,Int)] (64)
  for (i <- 0 to 7) {
    for(j <- 0 to 7) {
      cases(8 * i + j) = (i, j)
    }
  }
  /**Checks if case (x,y) is on the board. Useful to determine if a piece is dead, since those are in (-1,-1) */
  def on_board (x : Int, y : Int) = {
    (0 <= x) && (x <= 7) && (0 <= y) && (y <= 7);
  }
/**Converts an array to a string and hadh it. Used to store boards */
 def array_to_hashed_string (g : Array[Piece]) :Int = {
   var st = ""
   for (i <- 0 to g.size - 1){
     st += g(i).name + g(i).player.toString + g(i).pos_x.toString + g(i).pos_y.toString
     g(i).name match {
       case "king" => st+=g(i).asInstanceOf[King].has_moved
       case "rook" => st+=g(i).asInstanceOf[Rook].has_moved
       case _ => ()
     }
   }
   st.hashCode()
 }
 /**Checks if a list contains three identic elements */
 def contains_triplicates ( positions : List[Int] ) : Boolean = {
   var l = positions.sorted
   var cmpt = 0
   var old_x = -1
   for ( x <- l if cmpt < 2){
     if (old_x ==x){
       cmpt+=1
     }
     else {
       cmpt = 0
     }
     old_x =x
   }
   cmpt == 2
  }
  /**Returns the piece on coords(x,y) or None if there is none*/
  def piece_of_coord (x : Int, y : Int, g : Array[Piece], grid_id : Int) : Option[Piece] = {
    g find (p => p.pos_x == x && p.pos_y == y && p.grid == grid_id)
  }

  /**Checks for attacks on both kings and returns a list of attackers for each king.
    Used in the pre_move function*/
  def check_check (g : Array[Piece]) : Array[List[Piece]] = {
    var attacks = Array(Nil:List[Piece], Nil : List[Piece])
      for (i <- 0 to g.size - 1){
        var p = g(i)
        var (x, y, color) = (p.pos_x, p.pos_y, p.player)
        //Checking if the piece isn't dead :
        if ( Aux.on_board(x, y) && p.grid == Ksparov.curr_game.kings(1 - color).grid ){
          var x_king = Ksparov.curr_game.kings(1 - color).pos_x
          var y_king = Ksparov.curr_game.kings(1 - color).pos_y
          //Checking the validity of the move :
          if (p.checks_pre_move(x_king, y_king)){
            var (dir_x, dir_y) = p.get_dirs(x_king, y_king)
            //Checking if the piece can actually attack the king :
            var (clear, piece_on_arrival) = p.clear_path(x, y, x_king, y_king, g, dir_x, dir_y)
            if (clear) {
              attacks(1 - color) = attacks(1 - color):+p
            }
          }
    }
  }
  attacks
 }
}

/**The general class from which all pieces inherit*/
abstract class Piece (play : Int, x : Int, y : Int, grid_id : Int) {
  /**x coordinate of a piece */
  var pos_x = x
  /**y coordinate of a piece*/
  var pos_y = y
  /**Returns the coordinates of the piece*/
  def coords = (pos_x, pos_y)
  /**[Alice Chess] Moves the piece to the next grid*/
  def next_grid = {
    grid = (grid + 1) % (Ksparov.curr_game.nb_grid)
  }
  /**[Alice Chess] Moves the piece to the previous_grid grid*/
  def previous_grid = {
    grid = (grid + Ksparov.curr_game.nb_grid - 1) % (Ksparov.curr_game.nb_grid)
  }
  /**A string describing the type of piece (pawn, rook...). Has no capital letter */
  var name : String
  /**The path to the file containing the image for that piece*/
  var piece_path : String
  /**0 if the piece is blac, 1 if it is white*/
  var player = play
  /**1 if the player is black, -1 if it is white*/
  def player_to_direction = {
    if (play == 0){
      1
    }
    else {
      -1
    }
  }
  /**[Alice Chess] indicates the grid in which the piece is*/
  var grid : Int = grid_id
  /**The way the piece is allowed to move, independantly from the board*/
  def pattern(x_a : Int, y_a : Int) : Boolean

  /**Returns the directions of a movement (-1, 0 or 1) */
  def get_dirs (x_a : Int, y_a : Int) : (Int, Int) = {
    (math.signum(x_a - pos_x), math.signum(y_a - pos_y))
  }
  /**[Alice Chess] Checks if the corresponding case on the next grid is free */
  def mirror_free (x_a : Int, y_a : Int) = {
    if (Ksparov.curr_game.alice_chess) {
      Aux.piece_of_coord(x_a, y_a, Ksparov.curr_game.board, (grid + 1) % (Ksparov.curr_game.nb_grid)) == None
    } else {
      true
    }
  }

  /**Checks if basic conditions for a piece movement are satisfied */
  def checks_pre_move (x_a : Int, y_a : Int) = {
    pattern(x_a, y_a) && Aux.on_board(pos_x, pos_y) && Aux.on_board(x_a, y_a) && !(x_a == pos_x && y_a == pos_y) && mirror_free(x_a, y_a)
  }

  /**Recursive function that explores the path of a movement.
  The boolean indicates if the path is clear, and the piece is either the ennemy piece on arrival, an obstacle on the path, or None.*/
  def clear_path (x_d : Int, y_d : Int, x_a : Int, y_a : Int, g : Array[Piece], dir_x : Int, dir_y : Int) : (Boolean, Option[Piece]) = {
    //Getting next case
    var next_x = dir_x + x_d
    var next_y = dir_y + y_d
    var p = Aux.piece_of_coord (next_x, next_y, g, grid)
    //Base case
    if (x_a == next_x && y_a == next_y){p match {
      case None => (true,None) //arrival is free
      case _ if (p.get).player == player => (false, Some(p.get)) //Friendly piece on arrival
      case _ if (p.get).player != player => (true, Some(p.get)) //Ennemy piece on arrival
    }}
    //Recursive case
    else {p match {
      case None => clear_path(next_x, next_y, x_a, y_a,g ,dir_x ,dir_y) //recursive call
      case _ => (false,Some(p.get)) //There is an obstacle on the path
    }}
  }

  /**Auxiliary function for move : checks if a move is possible but doesn't modify the board
  Returns the validity of the move, the piece taken, and a list of the pieces attacking the opponent's king*/
  def pre_move(x_a : Int, y_a : Int, g : Array[Piece]) : (Boolean, Option[Piece], List[Piece]) = {

    //Checking if the arrival or the piece is on board and if the pattern is respected
    if (!(checks_pre_move(x_a, y_a))) {
      (false, Some(this), Nil)
    } else {
      //Checking the path and the destination
      var (dir_x, dir_y) = get_dirs (x_a, y_a)
      var (clear, p_arrival) = clear_path (pos_x, pos_y, x_a, y_a, g, dir_x, dir_y)

      if (!clear) {
        (false, p_arrival, Nil) //There is a friendly piece on the destination
      } else {
        /* We now have to simulate the movement to check if it is valid once applied.
        Since we do not want to modify the board, we save the current coordinates. */
        var (old_x, old_y) = coords
        var (old_x2, old_y2) = (0, 0)
        pos_x = x_a
        pos_y = y_a
        next_grid
        p_arrival match {
          case None => ();
          case _ => old_x2 = p_arrival.get.pos_x
            old_y2 = p_arrival.get.pos_y
            p_arrival.get.pos_x = (-1)
            p_arrival.get.pos_y = (-1)
        }

        //Checking if the king is attacked
        val checks1 = Aux.check_check(g)
        previous_grid
        val checks2 = Aux.check_check(g)

        //Restablishing previous position
        pos_x = old_x
        pos_y = old_y

        if (p_arrival != None) {
          p_arrival.get.pos_x = old_x2
          p_arrival.get.pos_y = old_y2
        }

        //Giving final answer based on attacks on king
        if (!(checks1 (player).isEmpty) || !(checks2 (player).isEmpty)) {
          (false, None, checks1 (player)) //The king is attacked and this move doesn't protect the king
        } else {
          (true, p_arrival, checks1 (1 - player))  //This move is valid
        }
      }
    }
  }

  /**Returns the validity of movement and the piece taken or the reason why the move is invalid. If the move is valid, applies it.**/
  def move (x_a : Int, y_a : Int, g : Array[Piece]) : Boolean = {
    var (move_ok, p_arrival, attackers) = pre_move (x_a, y_a, g)
    if (move_ok) { //If the move is valid, we apply the changes
      pos_x = x_a ;
      pos_y = y_a ;
      next_grid
      if (p_arrival != None) { //Killing the piece taken
        p_arrival.get.pos_x = (-1)
        p_arrival.get.pos_y= (-1)
        Ksparov.curr_game.nb_boring_moves = 0
      }
      else {
        Ksparov.curr_game.nb_boring_moves+=1
      }
      var king = Ksparov.curr_game.kings (1 - player) //Updating list of king's attackers
      king.attackers = attackers
      name match { //this is needed for castling
        case "rook" => this.asInstanceOf[Rook].has_moved = true
        case "king" => this.asInstanceOf[King].has_moved = true
        case _ => ()
      }
    }
    move_ok
  }

  /**Auxiliary function for possible_moves returning the possibility of a move */
  def aux (x : (Int, Int), g : Array[Piece]) = {
    var (i, j) : (Int, Int) = x
    var (clear, a , b) = pre_move (i, j, g)
    clear
  }

  /**Returns an array of all the cases the piece can go to */
  def possible_moves (g : Array[Piece]) = {
    Aux.cases.filter (aux (_, g))
  }
}

/**The Pawn class. Clear_path and move are overriden because of pawn's special moves*/
class Pawn (b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "pawn"
  var piece_path = "Pawn.png"
  /**A pawn can go one (or two if it is on its starting case) case forward and eventually in diagonal to attack another piece*/
  def pattern(x_a : Int, y_a : Int)={
    if (player == 1) {
      pos_x == x_a && (y_a == pos_y+1 || y_a == pos_y + 2 && pos_y == 1) || ( (y_a == pos_y + 1) && (math.abs(pos_x - x_a) == 1))
    }
    else {
      pos_x == x_a && (y_a == pos_y-1 || y_a == pos_y-2 && pos_y == 6) || ( (y_a == pos_y - 1) && (math.abs(pos_x - x_a) == 1))
    }
  }

  /**Pawns can only attack diagonally, and not while going forward*/
  override def clear_path (x_d : Int, y_d: Int, x_a : Int, y_a : Int, g : Array[Piece], dir_x : Int, dir_y : Int) : (Boolean, Option[Piece])={
    if (x_a == pos_x) { //the pawn isn't attacking, we just apply the super method
      var (clear, p_arrival) = super.clear_path(x_d, y_d, x_a, y_a, g, dir_x, dir_y)
      (clear && p_arrival == None, p_arrival)
    }
    else { //the pawn is attacking. But there is no need to call clear_path, as the destination is only one case away.
      var p : Option[Piece] = Aux.piece_of_coord(x_a, y_a, g, grid)
      //Getting last played move to check if en passant occurs
      var en_passant_ok = false
      if (Save.list_of_moves.nonEmpty){
      var (irock,prom,piece_prom,piece, attack, check, p1,p2) = Save.list_of_moves.head
      val deplacement = math.abs(p2._2-p1._2) // Has it moved two cases ?
      en_passant_ok = (!Ksparov.curr_game.alice_chess && piece == "" && deplacement == 2 && p1._1 == x_a && p2._2 == pos_y) // Is it now adjacent to the arrival ?
      }
       p match {
        case None if en_passant_ok => (true, Aux.piece_of_coord(x_a, y_a + player_to_direction , g, grid))
        case None => (false, None)
        case _ => (p.get.player != player, Some(p.get))
      }
    }
  }
  /**We override move to handle the case of promotion*/
  override def move (x_a : Int, y_a : Int, g : Array[Piece]) = {
    var move_ok = super.move( x_a, y_a, g)
    if (y_a ==  7 * player && move_ok ){
      Ksparov.curr_game.promoted_piece = this
      if (Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai) {
        Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai_promotion
      } else {
        DrawActions.enable_promotion (Ksparov.curr_game.curr_player)
      }
    }
    if (move_ok){
      Ksparov.curr_game.nb_boring_moves = 0
    }
    move_ok
  }
}

/**The Rook class*/
class Rook(b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "rook"
  var piece_path = "Rook.png"
  /**Indicates if the piece has already moved or not. Used for castling*/
  var has_moved = false
  def pattern (x_a : Int, y_a : Int) = {
    (x_a == pos_x) || (y_a == pos_y)
  }
}
/**The Knight class. clear_path is overriden*/
class Knight(b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "knight" //I still argue black knights should be called Batmans
  var piece_path = "Knight.png"
  def pattern (x_a : Int, y_a : Int)= { //A knight can reach a case if it is at distance 3 and not aligned with its position
    math.abs(pos_x - x_a) + math.abs(pos_y - y_a) == 3 && (pos_x != x_a) && (pos_y != y_a)
  }

  /**Knighs can jump over all other pieces, so we are jumping over the recursive case too*/
  override def clear_path(x_d:Int, y_d:Int, x_a:Int, y_a:Int, g:Array[Piece], dir_x:Int, dir_y:Int) : (Boolean, Option[Piece]) = {

    if (Ksparov.curr_game.alice_chess){
      var piece_arrival = Aux.piece_of_coord (x_a, y_a, g, (grid + 1) % (Ksparov.curr_game.nb_grid))
      if (piece_arrival != None) {
        (false, piece_arrival)
      }
    }
    var p = Aux.piece_of_coord(x_a,y_a,g, grid)
    p match {
      case None => (true,None)
      case _ if (p.get).player == player => (false,Some(p.get))
      case _ if (p.get).player != player => (true, Some(p.get))
    }
  }
}
/**The Bishop class*/
class Bishop (b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "bishop"
  var piece_path = "Bishop.png"
  def pattern(x_a : Int, y_a : Int) = {
    (pos_x - x_a)==(pos_y - y_a) || (pos_x - x_a)== (y_a - pos_y)
  }
}
/**The King class.Overrides for castling*/
class King (b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "king"
  var piece_path = "King.png"
  /**A list of the pieces attacking the king. Updated during at each move.*/
  var attackers : List[Piece] = Nil
  /**Indicates if the piece has already moved. Used for castling*/
  var has_moved : Boolean = false
  def attacked = attackers.nonEmpty
  def pattern(x_a : Int, y_a : Int)={
    (math.abs(pos_x - x_a) <= 1) && (math.abs(pos_y - y_a) <= 1)
  }

  /**An override is neccesary to handle the case of castling*/
  override def pre_move (x_a : Int, y_a : Int, g : Array[Piece]) : (Boolean, Option[Piece], List[Piece]) = {
    //Checking the basic conditions for castling
    if (!has_moved && !attacked && math.abs(pos_x - x_a) == 2 && pos_y == y_a && !Ksparov.curr_game.alice_chess) {
      //Getting corresponding rook.
      var dir_x = math.signum (x_a - pos_x)
      var x_rook = dir_x match {
        case 1 => 7
        case (-1) => 0
        case _ => -1
      }
      var rook = Aux.piece_of_coord(x_rook, pos_y, g, grid)
      //In case this is a queenside castling, checking the emptiness of the case next to the rook. If it is kingside, the case will be off board anyway.
      var right_neighbor = Aux.piece_of_coord(x_rook + 1, pos_y, g, grid)
      if ( (rook != None) && (rook.get.name == "rook") && !rook.get.asInstanceOf[Rook].has_moved && (right_neighbor == None)) { //Lazy evaluation FTW !
       //If the conditions are ok, we have to check if the king is not attacked during its movement. We use the super method for this.
        var (move_ok1, p_arrival1, l1) = super.pre_move(pos_x + dir_x, pos_y, g)
        pos_x += dir_x
        var (move_ok2, p_arrival2, l2) = super.pre_move(pos_x + dir_x, pos_y, g)
        pos_x -= dir_x
        (move_ok1 && move_ok2 && p_arrival1 == None && p_arrival2 == None, rook, l2)
      }
      else {
        (false, rook, Nil)
      }
    }
    else {
      super.pre_move (x_a, y_a, g)
    }
  }

  /**Since castling needs to move two pieces, we have to override move too*/
  override def move(x_a : Int, y_a : Int, g : Array[Piece]) : Boolean = {
    var (move_ok, p_arrival, attackers) = pre_move(x_a, y_a, g)
    if (move_ok && math.abs(pos_x - x_a) == 2) {
      pos_x = x_a ;
      pos_y = y_a ;
      var (x_rook, y_rook) = p_arrival.get.coords
      p_arrival.get.pos_x = x_rook match {
        case 0 => 3
        case 7 => 5
      }
      p_arrival.get.asInstanceOf[Rook].has_moved = true
      has_moved = true
      var king = Ksparov.curr_game.kings(1 - player)
      king.attackers = attackers
      if (move_ok) {
        Ksparov.curr_game.nb_boring_moves+=1
      }
      move_ok
    }
    else {
      super.move (x_a, y_a, g)
    }
  }
}
/**The Queen class*/
class Queen (b : Int, x0 : Int, y0 : Int, grid_id : Int) extends Piece (b, x0, y0, grid_id) {
  var name = "queen"
  var piece_path = "Queen.png"
  def pattern(x_a:Int,y_a:Int)={
    (x_a == pos_x) || (y_a == pos_y) || (pos_x - x_a) == (pos_y - y_a) || (pos_x - x_a) == (y_a - pos_y)
  }
}

/**This object contains methods to determine if the game is Nulle*/
object Nulle {
  /**Returns true only if the piece is still living and not a king */
  def is_living (p : Piece) : Boolean = {
    Aux.on_board(p.pos_x, p.pos_y) && p.name != "king"
  }

  /**Returns the pieces that are still on board, and not kings*/
  def living_pieces (g : Array[Piece]) ={
    g.filter(is_living)
  }
  /**Checks for various nulle finals */
  def trivial_nulle (g : Array[Piece]) : Boolean = {
    var reduced_g = living_pieces (g)
    reduced_g.size match {
      case 0  => true
      case 1  => var remaining_piece = reduced_g(0).name ; (remaining_piece == "knight" || remaining_piece == "bishop")
      case 2 =>  var p1 = reduced_g(0) ; var p2 = reduced_g(1) ;
                (p1.name =="bishop" && p2.name == "bishop" && p2.player != p1.player && p1.pos_x+8*p1.pos_y == p2.pos_x +8*p2.pos_y)
     case _ => false
    }
  }
}

/**This object contains methods to determine if there is a Checkmate*/
object Checkmate {
 /**Returns the possibility of the move : Piece p to (x_a,y_a) in game g */
  def move_is_possible(p : Piece, x_a : Int, y_a : Int, g :Array[Piece] ) : Boolean = {
    var (possible, a, b) = p.pre_move(x_a, y_a, g)
    possible
  }
  /**Checks if player pl has lost the game. */
  def check_mate(g : Array[Piece], pl : Int) : Boolean = {
    var king = Ksparov.curr_game.kings(pl)
    var (x_king, y_king) = king.coords
    var checkmate = king.attacked
    //Checking if the king can escape the attack on its own.
    for (i <- (-1) to 1 ; j<-(-1) to 1 if checkmate){
      checkmate = checkmate && !(move_is_possible (king, x_king + i, y_king + j, g))
    }
    var attackers = king.attackers
    if (checkmate && (attackers.size == 1) ) { //If there is only one attacker, a piece can be put between the attacker and the king, to defend it.
      var (x_att,y_att) = attackers(0).coords
      for (p <- g if checkmate && p.player == pl){
        var (x, y) = (x_att, y_att)
        var (dir_x, dir_y) = attackers(0).get_dirs(x_king,y_king)
        if (attackers(0).name != "knight") { //If the attacker is not a knight, we check each case of the attack.
        while (checkmate && ! (x == x_king && y == y_king)) {
          checkmate = checkmate & !move_is_possible(p, x, y, g)
          x += dir_x
          y += dir_y
        }
      }
      else { //If the attacker is a knight, it has to be taken down.
        checkmate = checkmate & !move_is_possible(p, x, y, g)
      }
    }
  }
  checkmate
  }
}
