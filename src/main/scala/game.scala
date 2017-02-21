import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.matching.Regex

class Player(n:Int) {
  val id : Int = n
  var moved = false
  var ai = false
  def getmove = ()
}

class Human(n : Int) extends Player(n : Int) {

  /* Take a couple of positions and return true if the piece is owned by the player */
  def isHis(x : Int, y : Int) : Boolean = {
    var done = false 
    /* We run from 0 to 15 for the player 1 (white) and from 16 to 31 for the player 0 (black) */
    for (i <- (1 - id) * 16 to (2 - id) * 16 - 1) {
      done = done || ((Ksparov.board(i).pos_x == x) && (Ksparov.board(i).pos_y == y))
    }
    return done
  }

  override def getmove : Unit = {
    /* Converting the case selected into x and y */
    var x = Constants.selected_case % 8
    var y = Constants.selected_case / 8
    /* First click */
      if (isHis(x,y)) {
        /* If so validate the first clic and open the second click*/
        Constants.first_choice_done = true
        /* Then select the piece of position and draw possible moves */
        Ksparov.get_piece_of_pos(x,y)
        DrawActions.clear_possible_moves
        DrawActions.draw_possible_moves(Ksparov.board(Constants.selected_piece).possible_moves(Ksparov.board), x, y)
      } else {
    /* Second click */
    if (Constants.first_choice_done) {
      /* The variable valid check if the move is valid, and p is the optionnal piece taken */
      var (valid,p) = Ksparov.board(Constants.selected_piece).move(x,y,Ksparov.board)
      if (valid) {
        /* If the move is valid, apply the new board */
        DrawActions.draw_game_board(Ksparov.board)
        Constants.players(Constants.curr_player).moved = true
      /* If the move is invalid, research for a first click */
      } else {
        Constants.first_choice_done = false
        DrawActions.clear_possible_moves
      }
    }
  }
    /* Clear the possibles moves coloring */
  }
}

object Constants {

  /* Defining the dimension for the cases */
  val dim_small = new Dimension (80, 80)
  val dim_big = new Dimension (240, 80)
  val nb_case_border = 1
  val nb_case_board = 8
  var nb_case = nb_case_board + 2 * nb_case_border

  /* Defining the path to find every resource used in the programm */
  var resources_path = "src/main/resources/"
  var pieces_path = ""
  var small_texture_path = ""
  var text_color = Color.black
  val pattern = new Regex("\\d")

  def apply_parameters = {
    var lines = new Array[String](3)
    var i = 0
    for (line <- Source.fromFile("src/main/resources/Parameters").getLines) {
      lines (i) = line.toString
      i += 1
    }
    pieces_path = "Pieces/" + lines(1) + "/"
    small_texture_path = "Texture_small_" + lines(2) + ".png"
    text_color = Color.black
    lines(2).toInt match {
      case 1 => text_color = Color.white
      case 2 => text_color = Color.white
      case 3 => text_color = Color.black
      case 4 => text_color = Color.black
      case 5 => text_color = Color.red 
    }
  }

  def write_parameters (piece : String, texture : String) {
    var writer = new PrintWriter(new File ("src/main/resources/Parameters"))
    writer.write ("Lines of this file : 1 - Pieces, 2 - Texture\n")
    writer.write (piece + "\n")
    writer.write (texture + "\n")
    writer.close

  }
  
  /* Game variables: the case selected by the player, the type of setup for the game
     and arrays for the game board and the dead pieces */
  var selected_case = 0
  var selected_piece = -1
  var game_type = 0
  var grid_cases = new Array[DrawBoard.Case] (nb_case_board * nb_case_board)
  var dead_pieces = Array(new Array[Int](5), new Array[Int](5))
  var kings = new Array[King](2)
  var players = new Array[Player](2)
  var curr_player = 1
  var first_choice_done = false
  var game_nulle = false
  var game_won = false

  var message_drawer = new DrawBoard.MessageDrawer ("")
}

object Ksparov {
  Constants.apply_parameters

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
        board((1-p)*16 + i) = new Pawn(p,i,1+(1-p)*5)
      }
      board( 8 + (1-p)*16) = new Rook(p,0, (1-p)*7)
      board( 9 + (1-p)*16) = new Rook(p,7,(1-p)*7)
      board( 10 + (1-p)*16) = new Knight(p,1,(1-p)*7)
      board( 11 + (1-p)*16) = new Knight(p,6,(1-p)*7)
      board( 12 + (1-p)*16) = new Bishop(p,2,(1-p)*7)
      board( 13 + (1-p)*16) = new Bishop(p,5,(1-p)*7)
      board( 14 + (1-p)*16) = Constants.kings(p)
      board( 15 + (1-p)*16) = new Queen(p,3,(1-p)*7)
    }
  }

  /* Return the index in the game_board of the piece of position (x, y) */
  def get_piece_of_pos (x : Int, y : Int) {
    for (i <- 0 to 31){
      if (Ksparov.board(i).pos_x == x && Ksparov.board(i).pos_y == y){
        Constants.selected_piece = i
      }
    }
  }

  def play_move(x : Int, y : Int) {
    /* Checking if the game has been won */
    if (Constants.game_won || Constants.game_nulle) {
      /* Don't do anything, just wait for other button */
    } else {
      Constants.players(Constants.curr_player).getmove
      if (Constants.players(Constants.curr_player).moved) {
        Constants.players(Constants.curr_player).moved = false
        /* Check if there is a mate after the move */
        if (Checkmate.check_mate (Ksparov.board, 1 - Constants.curr_player)) {
          /* If so, finish the game */
          DrawActions.draw_messages (6)
          Constants.game_won = true
        } else {
          if (Constants.game_nulle) {
            DrawActions.draw_messages (7)
          } else {
          /* Else check if there is check */
          if (Constants.kings(1 - Constants.curr_player).attacked) {
            DrawActions.draw_messages (5)
          } else {
            DrawActions.draw_messages (4)
          }
          Constants.curr_player = 1 - Constants.curr_player
          Constants.first_choice_done = false
        }}
      }
      if (Constants.players(Constants.curr_player).ai && Constants.game_type != 3) {
        play_move (0, 0)
      }
    }
  }

  def init_game(n : Int) {
    Constants.kings = Array(new King (0, 4, 7), new King (1, 4, 0))
    Ksparov.init_board()
    DrawActions.draw_game_board(Ksparov.board)
    n match {
      case 1 =>
        Constants.players(0) = new Human(0)
        Constants.players(1) = new Human(1)
        DrawActions.draw_messages (0)
      case 2 =>
        if (scala.util.Random.nextInt(2) == 0) {
          Constants.players(0) = new Human(0)
          Constants.players(1) = new AI(1)
        DrawActions.draw_messages (1)
        } else {
          Constants.players(1) = new Human(1)
          Constants.players(0) = new AI(0)
        DrawActions.draw_messages (2)
         }
      case 3 =>
        Constants.players(1) = new AI(1)
        Constants.players(0) = new AI(0)
        DrawActions.draw_messages (3)
    }
    Constants.game_won = false
    Constants.game_nulle = false
    Constants.curr_player = 1 
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
