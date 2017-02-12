import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source

class Player(n:Int) {
  val id:Int = n
  def getmove = ()
}

class Human(n:Int) extends Player(n:Int) {
}

object Constants {
  /* Reading the defaults parameters form the parameters file */
  var lines = new Array[String](4)
  var i = 0
  for (line <- Source.fromFile("src/main/resources/Parameters").getLines) {
    lines (i) = line.toString
    println (lines(i))
    i += 1
  }

  /* Defining the dimension for the cases */
  val dim_small = new Dimension (80, 80)
  val dim_big = new Dimension (240, 80)
  val nb_case_border = 1
  val nb_case_board = 8
  var nb_case = nb_case_board + 2 * nb_case_border

  /* Defining the path to find every resource used in the programm */
  var resources_path = "src/main/resources/"
  var pieces_path = "Pieces/" + lines(0) + "/"
  var small_texture_path = "Texture_small_" + lines(1) + ".png"

  /* Game variables: the case selected by the player, the type of setup for the game
     and arrays for the game board and the dead pieces */
  var selected_case = 0
  var game_type = 0
  var grid_cases = new Array[String](nb_case_board*nb_case_board)
  var dead_pieces = Array(new Array[Int](5), new Array[Int](5))
}

object Ksparov {

  var frame = new MainFrame {
    title = "Ksparov"
    contents = new DrawMenu.Menu
  }

  var board : Array[Piece] = new Array[Piece](32)

  var joueur1 : Player = new Player(1)
  var joueur0 : Player = new Player(0)


  def init_board(){
    for (p <- 0 to 1) {
      for(i <- 0 to 7) {
        board((1-p)*16 + i) = new Pawn(p,1+p*5,i)
        }
      board( 8 + (1-p)*16) = new Rook(p,p*7,0)
      board( 9 + (1-p)*16) = new Rook(p,p*7,7)
      board( 10 + (1-p)*16) = new Knight(p,p*7,1)
      board( 11 + (1-p)*16) = new Knight(p,p*7,6)
      board( 12 + (1-p)*16) = new Bishop(p,p*7,2)
      board( 13 + (1-p)*16) = new Bishop(p,p*7,5)
      board( 14 + (1-p)*16) = new King(p,p*7,4)
      board( 15 + (1-p)*16) = new Queen(p,p*7,3)
    }
  }

  def init_game(n : Int){
    n match {
      case 1 => 
                Ksparov.init_board()
                DrawActions.draw_game_board(Ksparov.board)
                joueur1 = new Human(1)
                joueur0 = new Human(0)
      case _ => ()
    }
  }

  /*On ne fait qu'une application, c'est simplement que l'on change de 
  frame pour changer de vue. On ne touche donc pas à Application mais que à frame !*/
  var application = new SimpleSwingApplication {
    def top = frame
  }

  def main(argv : Array[String]) {
    application.main(Array())
  }
}
