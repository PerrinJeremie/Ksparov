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
  var nb_case = nb_case_board + 2*nb_case_border
  var resources_path = "src/main/resources/"
  var pieces_path = "Pieces/1/"
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

  var board : Array[Piece] = new Array[Piece](32)

  def init_board(){
    for (p <- 0 to 1) {
      for(i <- 1 to 8) {
        board((1-p)*16 + (i-1)) = new Pawn("pawn",p,2+(1-p)*5,i)
        }
      board( 8 + (1-p)*16) = new Rook("rook",p,(1-p)*8,1)
      board( 9 + (1-p)*16) = new Rook("rook",p,(1-p)*8,8)
      board( 10 + (1-p)*16) = new Knight("knight",p,(1-p)*8,2)
      board( 11 + (1-p)*16) = new Knight("knight",p,(1-p)*8,7)
      board( 12 + (1-p)*16) = new Bishop("bishop",p,(1-p)*8,3)
      board( 13 + (1-p)*16) = new Bishop("bishop",p,(1-p)*8,6)
      board( 14 + (1-p)*16) = new Queen("queen",p,(1-p)*8,4+(1-p))
      board( 15 + (1-p)*16) = new Rook("king",p,(1-p)*8,5 - (1-p))
    }

  }
  /*On ne fait qu'une application, c'est simplement que l'on change de 
  frame pour changer de vue. On ne touche donc pas à Application mais que à frame !*/
  var application = new SimpleSwingApplication {
    def top = frame
  }

  def main(argv : Array[String]) {
    application.main(Array())
    Ksparov.init_board()
    for (i<-0 to Ksparov.board.length - 1){
      println((Ksparov.board(i).pos_x - 1) * 8 + (Ksparov.board(i).pos_y - 1))
    }
    /*DrawActions.draw_game_board(Ksparov.board)*/
  }
}
