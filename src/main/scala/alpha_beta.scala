
/** Stores the different evaluation functions */
object Evals {

  /** Simple board evaluation function as found at : https://chessprogramming.wikispaces.com/Simplified+evaluation+function */
  object Simple_Eval {

    /** Values of pieces in the order : P,N,B,R,Q,K */
    val piece_val : Array[Int] = Array(100,320,330,500,900,20000)

    /** Values for pawn position */
    val pawn_pos : Array[Array[Int]] =
      Array(Array(0,  0,  0,  0,  0,  0,  0,  0),
        Array(50, 50, 50, 50, 50, 50, 50, 50),
        Array(10, 10, 20, 30, 30, 20, 10, 10),
        Array(5,  5, 10, 25, 25, 10,  5,  5),
        Array(0,  0,  0, 20, 20,  0,  0,  0),
        Array(5, -5,-10,  0,  0,-10, -5,  5),
        Array(5, 10, 10,-20,-20, 10, 10,  5),
        Array(0,  0,  0,  0,  0,  0,  0,  0))

    /** Values for knight position */
    val knight_pos : Array[Array[Int]] =
      Array(Array(-50,-40,-30,-30,-30,-30,-40,-50),
        Array(-40,-20,  0,  0,  0,  0,-20,-40),
        Array(-30,  0, 10, 15, 15, 10,  0,-30),
        Array(-30,  5, 15, 20, 20, 15,  5,-30),
        Array(-30,  0, 15, 20, 20, 15,  0,-30),
        Array(-30,  5, 10, 15, 15, 10,  5,-30),
        Array(-40,-20,  0,  5,  5,  0,-20,-40),
        Array(-50,-40,-30,-30,-30,-30,-40,-50))

    /** Values for bishop position */
    val bishop_pos : Array[Array[Int]] =
      Array(Array(-20,-10,-10,-10,-10,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5, 10, 10,  5,  0,-10),
        Array(-10,  5,  5, 10, 10,  5,  5,-10),
        Array(-10,  0, 10, 10, 10, 10,  0,-10),
        Array(-10, 10, 10, 10, 10, 10, 10,-10),
        Array(-10,  5,  0,  0,  0,  0,  5,-10),
        Array(-20,-10,-10,-10,-10,-10,-10,-20))

    /** Values for rook position */
    val rook_pos : Array[Array[Int]] =
      Array(Array(0,  0,  0,  0,  0,  0,  0,  0),
        Array(5, 10, 10, 10, 10, 10, 10,  5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array(-5,  0,  0,  0,  0,  0,  0, -5),
        Array( 0,  0,  0,  5,  5,  0,  0,  0))

    /** Values for queen position */
    val queen_pos : Array[Array[Int]] =
      Array(Array(-20,-10,-10, -5, -5,-10,-10,-20),
        Array(-10,  0,  0,  0,  0,  0,  0,-10),
        Array(-10,  0,  5,  5,  5,  5,  0,-10),
        Array( -5,  0,  5,  5,  5,  5,  0, -5),
        Array(  0,  0,  5,  5,  5,  5,  0, -5),
        Array(-10,  5,  5,  5,  5,  5,  0,-10),
        Array(-10,  0,  5,  0,  0,  0,  0,-10),
        Array(-20,-10,-10, -5, -5,-10,-10,-20))

    /** Values for king position */
    val king_pos : Array[Array[Int]] =
      Array(Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-30,-40,-40,-50,-50,-40,-40,-30),
        Array(-20,-30,-30,-40,-40,-30,-30,-20),
        Array(-10,-20,-20,-20,-20,-20,-20,-10),
        Array( 20, 20,  0,  0,  0,  0, 20, 20),
        Array(20, 30, 10,  0,  0, 10, 30, 20))

    /** Evaluates the board for a given player p
      *  @param board The current board to evaluate
      *  @param p The player for which we evaluate the given board 
      */
    def evaluate(board : Array[Piece], player : Int) : Int = {

      var sum = 0
      
      for (i <- 0 to 31){
        
        var npiece =
          board(i).name match {
            case  "pawn" => 0
            case  "knight" => 1
            case  "bishop" => 2
            case  "rook" => 3
            case  "queen" => 4
            case  "king" => 5
          }

        var p = board(i).player

        var xpos = p*board(i).pos_x + (1-p)*(7 - board(i).pos_x)
        var ypos = p*(7 - board(i).pos_y) + (1-p)*board(i).pos_y

        if ( board(i).pos_x >= 0 && board(i).pos_y >= 0 ) {
          var c =  (1 - 2*Math.abs(p-player))
          sum += c*piece_val(npiece)
          sum += c*(npiece match {
            case 0 => pawn_pos(ypos)(xpos)
            case 1 => knight_pos(ypos)(xpos)
            case 2 => bishop_pos(ypos)(xpos)
            case 3 => rook_pos(ypos)(xpos)
            case 4 => queen_pos(ypos)(xpos)
            case 5 => king_pos(ypos)(xpos)
          })
        }

      }
      return sum

    }

  }


}

/** Contains the alphabeta functions which implement the alphabeta algorithm*/
object AlphaBeta {

  /** Stores the evluation function used, so as to be able to change it */
  var evaluate : (Array[Piece],Int) => Int = Evals.Simple_Eval.evaluate _

  /** The player which promotes in the algorithms, used only for simulation */
  var playerprom : Int = 0

  /** Saves the number of boring moved at the beginning of the evaluation */
  var save_boring : Int = 0

  /** The main alphabeta function which calls the recursive subroutine : alphaBetaMax
    * @param board the initial board
    * @param player the id of the player 
    * @param depth the depth to research 
    */
  def alphabeta(board : Array[Piece], player : Int, depth : Int) : Array[(Int,Int)] = {
    playerprom = player 

    save_boring =  Ksparov.curr_game.nb_boring_moves

    var i = alphaBetaMax(board,-1000000,1000000, player, depth)._2

    Ksparov.curr_game.nb_boring_moves = save_boring

    return i
  }

  /** The max part of the alphabeta algorithm
    * @param board the initial board
    * @param alpha current lower bound
    * @param beta current higher bound
    * @param player the id of the player 
    * @param depth the depth to research 
    */
  def alphaBetaMax( board : Array[Piece],alpha : Int, beta : Int, player : Int, depth : Int) : (Int,Array[(Int,Int)]) = {
    var old_pos : Array[(Int,Int,Int)] = new Array[(Int,Int,Int)](32) 
    var pawns : Array[Boolean] = new Array[Boolean](8)
    var score = 0
    var alphap = alpha
    var betap = beta 
    var have_moved_init : (Boolean,Boolean,Boolean,Boolean,Boolean,Boolean) = (false,false,false,false,false,false)
    var attackers : Array[List[Piece]] = new Array[List[Piece]] (2)
    var mv : Array[(Int,Int)] = Array((0,0),(-1,-1))
    if ( depth == 0 ){
      return (evaluate(board,player), mv)
    } else {
    for ( i <- (1-player)*16 to 15 + (1-player)*16){
      if (board(i).pos_x >= 0){
        var tab = board(i).possible_moves(board)
        for (j <- 0 to tab.length - 1){

          for (k <- 0 to 31)  {
            if (k >= (1-player)*16  && k <= (1-player) * 16 + 7){
                pawns(k - (1-player)*16) = board(k).name match {
                  case "pawn" => true 
                  case _ =>  false }
            }
            old_pos(k) = (board(k).pos_x,board(k).pos_y,board(k).grid)
          }

          have_moved_init = (board(8).asInstanceOf[Rook].has_moved, board(14).asInstanceOf[King].has_moved, board(9).asInstanceOf[Rook].has_moved, board(24).asInstanceOf[Rook].has_moved,board(30).asInstanceOf[King].has_moved,board(25).asInstanceOf[Rook].has_moved)

          attackers(0) = Ksparov.curr_game.kings(0).attackers
          attackers(1) = Ksparov.curr_game.kings(1).attackers

          playerprom = player

          board(i).move(tab(j)._1,tab(j)._2,board)
          score = alphaBetaMin( board,alphap,betap, 1 - player, depth - 1 )._1

          for (k <- 0 to 31)  {
            if (k >= (1-player)*16  && k <= (1-player) * 16 + 7 && pawns(k - (1-player)*16)){
              board(k) = new Pawn (player,old_pos(k)._1, old_pos(k)._2,old_pos(k)._3)
            } else {
              board(k).pos_x = old_pos(k)._1
              board(k).pos_y = old_pos(k)._2
              board(k).grid = old_pos(k)._3
            }
          }

          Ksparov.curr_game.board(8).asInstanceOf[Rook].has_moved = have_moved_init._1
          Ksparov.curr_game.board(14).asInstanceOf[King].has_moved = have_moved_init._2
          Ksparov.curr_game.board(9).asInstanceOf[Rook].has_moved = have_moved_init._3
          Ksparov.curr_game.board(24).asInstanceOf[Rook].has_moved = have_moved_init._4
          Ksparov.curr_game.board(30).asInstanceOf[King].has_moved = have_moved_init._5
          Ksparov.curr_game.board(25).asInstanceOf[Rook].has_moved = have_moved_init._6

          Ksparov.curr_game.kings(0).attackers = attackers(0)
          Ksparov.curr_game.kings(1).attackers = attackers(1) 

          if( score >= betap ){
            return (betap,Array(board(i).coords,tab(j)))
          }
          if( score > alphap ){
            alphap = score
            mv = Array(board(i).coords,tab(j))
          }
        }
      }
    }
    return (alphap,mv)
    }
  }


  /** The min part of the alphabeta algorithm
    * @param board the initial board
    * @param alpha current lower bound
    * @param beta current higher bound
    * @param player the id of the player 
    * @param depth the depth to research 
    */
  def alphaBetaMin( board : Array[Piece],alpha : Int, beta : Int, player : Int, depth : Int) : (Int,Array[(Int,Int)]) = {
    var old_pos : Array[(Int,Int,Int)] = new Array[(Int,Int,Int)](32) 
    var pawns : Array[Boolean] = new Array[Boolean](8)
    var score = 0
    var alphap = alpha
    var betap = beta 
    var have_moved_init : (Boolean,Boolean,Boolean,Boolean,Boolean,Boolean) = (false,false,false,false,false,false)
    var attackers : Array[List[Piece]] = new Array[List[Piece]] (2)
    var mv : Array[(Int,Int)] = Array((0,0),(-1,-1))
    if ( depth == 0 ){
      return ( -evaluate(board,player), mv)
    } else {
    for ( i <- (1-player)*16 to 15 + (1-player)*16){
      if (board(i).pos_x >= 0){
        var tab = board(i).possible_moves(board)
        var name = board(i).name
        for (j <- 0 to tab.length - 1){
          for (k <- 0 to 31)  {
            if (k >= (1-player)*16  && k <= (1-player) * 16 + 7){
                pawns(k - (1-player)*16) = board(k).name match {
                  case "pawn" => true 
                  case _ =>  false }
            }
            old_pos(k) = (board(k).pos_x,board(k).pos_y,board(k).grid)
          }

          have_moved_init = (board(8).asInstanceOf[Rook].has_moved, board(14).asInstanceOf[King].has_moved, board(9).asInstanceOf[Rook].has_moved, board(24).asInstanceOf[Rook].has_moved,board(30).asInstanceOf[King].has_moved,board(25).asInstanceOf[Rook].has_moved)

          attackers(0) = Ksparov.curr_game.kings(0).attackers
          attackers(1) = Ksparov.curr_game.kings(1).attackers

          playerprom = player

          board(i).move(tab(j)._1,tab(j)._2,board)
          score = alphaBetaMax( board,alphap,betap, 1 - player, depth - 1 )._1

          for (k <- 0 to 31)  {
             if (k >= (1-player)*16  && k <= (1-player) * 16 + 7 &&  pawns(k - (1-player)*16)){
              board(k) = new Pawn (player,old_pos(k)._1, old_pos(k)._2,old_pos(k)._3)
            } else {
              board(k).pos_x = old_pos(k)._1
              board(k).pos_y = old_pos(k)._2
              board(k).grid = old_pos(k)._3
            }
          }    

          Ksparov.curr_game.board(8).asInstanceOf[Rook].has_moved = have_moved_init._1
          Ksparov.curr_game.board(14).asInstanceOf[King].has_moved = have_moved_init._2
          Ksparov.curr_game.board(9).asInstanceOf[Rook].has_moved = have_moved_init._3
          Ksparov.curr_game.board(24).asInstanceOf[Rook].has_moved = have_moved_init._4
          Ksparov.curr_game.board(30).asInstanceOf[King].has_moved = have_moved_init._5
          Ksparov.curr_game.board(25).asInstanceOf[Rook].has_moved = have_moved_init._6

          Ksparov.curr_game.kings(0).attackers = attackers(0)
          Ksparov.curr_game.kings(1).attackers = attackers(1) 

          if( score <= alphap ){
            return (alphap,Array(board(i).coords,tab(j)))
          }
          if( score < betap ){
            betap = score
            mv = Array(board(i).coords,tab(j))
          }
        }
      }
      }
      return (betap,mv)
    }
  }

}
