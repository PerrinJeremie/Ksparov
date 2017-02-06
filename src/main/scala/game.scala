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
  val dim_case = new Dimension (80, 80)
  val nb_case_border = 1
  val nb_case_board = 8
  val nb_case = nb_case_board + 2*nb_case_border
  var selected_case = 0
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
    var board_test = Array[Piece] (piece1, piece2)
    application.main(Array())
  }
}
