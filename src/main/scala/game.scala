abstract class Player {
  var id:Int = 0
  def getmove : (Int,Int)
}


object Ksparov{
  def main(argv : Array[String]){
    val frame = new Board
    val piece1 = new Pawn("pawn",1,4,4)
    val piece2 = new Bishop("bishop",1,3,2)
    var board_test = Array[Piece] (piece1, piece2)
    frame.main(Array())
    frame.draw_game_board(board_test)
  }
}
