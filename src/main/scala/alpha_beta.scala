
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

/**
  * 
  */
object AlphaBeta {

  var evaluate : (Array[Piece],Int) => Int = Evals.Simple_Eval.evaluate _

  var playerprom : Int = 0

  def alphabeta(board : Array[Piece], player : Int, depth : Int) : Array[(Int,Int)] = {
    playerprom = player 
    var i = alphaBetaMax(board,-1000000,1000000, player, depth)._2
    return i
  }

  def undo_move (mv : (Int, Boolean, Int, Option[Piece], (Int,Int), (Int,Int),(Int,Int)),board : Array[Piece], player : Int): Unit = {
    if (mv._1 != 0) {
      board((1-player)*16 + 14).pos_x = 4
      if (mv._1 == 1){
        board((1-player)*16 + 8).pos_x = 0
      } else{
        board((1-player)*16 + 9).pos_x = 7
      }
    } else {
      if (mv._2){
        board(mv._3) =  new Pawn(player,mv._5._1,mv._5._2,0)
      } else {
        board(mv._3).pos_x = mv._5._1
        board(mv._3).pos_y = mv._5._2
      }
      mv._4 match{
        case None => ()
        case Some(p) =>
            p.pos_x = mv._7._1
            p.pos_y = mv._7._2
          }
      }

    }
  

  def alphaBetaMax( board : Array[Piece],alpha : Int, beta : Int, player : Int, depth : Int) : (Int,Array[(Int,Int)]) = {
    var old_pos : Array[(Int,Int,Int)] = new Array[(Int,Int,Int)](32) 
    var score = 0
    var alphap = alpha
    var betap = beta 
    var mv : Array[(Int,Int)] = Array((0,0),(0,0))
    if ( depth == 0 ){
      return (evaluate(board,player), mv)
    } else {
    for ( i <- (1-player)*16 to 15 + (1-player)*16){
      if (board(i).pos_x >= 0){
        var tab = board(i).possible_moves(board)
        for (j <- 0 to tab.length - 1){
          for (k <- 0 to 31)  {
            old_pos(k) = (board(k).pos_x,board(k).pos_y,board(k).grid)
          }
          playerprom = 1 - player
          board(i).move(tab(j)._1,tab(j)._2,board)
          playerprom = player
          score = alphaBetaMin( board,alphap,betap, 1 - player, depth - 1 )._1
          for (k <- 0 to 31)  {
            if (k >= (1-player)*16  && k <= (1-player) * 16 + 7 && board(k).name != "pawn"){
              board(k) = new Pawn (player,old_pos(k)._1, old_pos(k)._2,old_pos(k)._3)
            } else {
              board(k).pos_x = old_pos(k)._1
              board(k).pos_y = old_pos(k)._2
              board(k).grid = old_pos(k)._3
            }
          }
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

  def alphaBetaMin( board : Array[Piece],alpha : Int, beta : Int, player : Int, depth : Int) : (Int,Array[(Int,Int)]) = {
    var old_pos : Array[(Int,Int,Int)] = new Array[(Int,Int,Int)](32) 
    var score = 0
    var alphap = alpha
    var betap = beta 
    var mv : Array[(Int,Int)] = Array((0,0),(0,0))
    if ( depth == 0 ){
      return ( -evaluate(board,player), mv)
    } else {
    for ( i <- (1-player)*16 to 15 + (1-player)*16){
      if (board(i).pos_x >= 0){
        var tab = board(i).possible_moves(board)
        var name = board(i).name
        for (j <- 0 to tab.length - 1){
          for (k <- 0 to 31)  {
            old_pos(k) = (board(k).pos_x,board(k).pos_y,board(k).grid)
          }
          playerprom = 1 - player
          board(i).move(tab(j)._1,tab(j)._2,board)
          playerprom = player
          score = alphaBetaMax( board,alphap,betap, 1 - player, depth - 1 )._1
          for (k <- 0 to 31)  {
             if (k >= (1-player)*16  && k <= (1-player) * 16 + 7 && board(k).name != "pawn"){
              board(k) = new Pawn (player,old_pos(k)._1, old_pos(k)._2,old_pos(k)._3)
            } else {
              board(k).pos_x = old_pos(k)._1
              board(k).pos_y = old_pos(k)._2
              board(k).grid = old_pos(k)._3
            }
          }    
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
