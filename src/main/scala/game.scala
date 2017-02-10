import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._

abstract class Player {
  var id:Int = 0
  def getmove : (Int,Int)
}

object Constants {
  val dim_small = new Dimension (80, 80)
  val dim_big = new Dimension (240, 80)
  val nb_case_border = 1
  val nb_case_board = 8
  var nb_case = nb_case_board + 2 * nb_case_border
  var resources_path = "src/main/resources/"
  var pieces_path = "Pieces/2/"
  var small_texture_path = "Texture_small_2.png"
  var big_texture_path = "Texture_big_2.png"
  var selected_case = 0
  var game_type = 0
}

object Ksparov {

  var frame = new MainFrame {
    title = "Ksparov"
    contents = new DrawMenu.Menu
  }

  /*On ne fait qu'une application, c'est simplement que l'on change de 
  frame pour changer de vue. On ne touche donc pas à Application mais que à frame !*/
  var application = new SimpleSwingApplication {
    def top = frame
  }

  def main(argv : Array[String]) {
    
    val piece1 = new Pawn("pawn",1,4,4)
    val piece2 = new Bishop("bishop",1,3,2)
    val piece3 = new King ("king", 2, 5, 5)
    val piece4 = new Rook ("rook", 2, 2, 0)
    val piece5 = new Queen ("queen", 1, 1, 1)
    val piece6 = new Pawn("pawn", 2, -1, -1)
    var board_test = Array[Piece] (piece1, piece2, piece3, piece5, piece4, piece6)
    application.main(Array())
    DrawActions.draw_game_board(board_test)
  }
}
