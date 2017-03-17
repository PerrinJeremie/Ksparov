import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.matching.Regex
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar

/* The class that defines a player, it will be extended in Human just below and in AI in rules.scala. */
abstract class Player(n:Int) {
  val id : Int = n
  var moved = false
  /* ai is true if and only if the player is an artifical inteligence. */
  var ai = false
  def getmove : Unit
  def check_pat : Boolean
  def ai_promotion : Unit
}

/* The class for a human player, defines some method and mainly the getmove method. */
class Human(n : Int) extends Player(n : Int) {

  /* Take a couple of positions and return true if the piece is owned by the player. */
  def isHis(x : Int, y : Int) : Boolean = {
    var done = false
    /* We run from 0 to 15 for the player 1 (white) and from 16 to 31 for the player 0 (black) */
    for (i <- (1 - id) * 16 to (2 - id) * 16 - 1) {
      done = done || ((Ksparov.board(i).pos_x == x) && (Ksparov.board(i).pos_y == y))
    }
    return done
  }

  /* Select and apply a move. */
  override def getmove : Unit = {
    /* Converting the case selected into x and y. */
    var x = Constants.selected_case % 8
    var y = Constants.selected_case / 8
    var grid = Constants.selected_grid
    /* First click, checking if the piece selected is own by the player. */
    if (isHis(x,y)) {
      /* If so validate the first clic and open the second click. */
      Constants.first_choice_done = true
      /* Then select the piece of position and draw possible moves. */
      Ksparov.get_piece_of_pos(x, y, grid)
      DrawActions.clear_possible_moves
      DrawActions.draw_possible_moves(Ksparov.board(Constants.selected_piece).possible_moves(Ksparov.board), x, y, 0)
    } else {
      /* Because we cannot move on a piece own by the player, we can enter in the else here.
         Second click, checking if the first has been done. */
      if (Constants.first_choice_done) {
        /* loads part of the move to be added but has to wait for promotion information */
        Save.add_move1( Constants.selected_piece, (x,y))
        /* The variable valid check if the move is valid, and p is the optionnal piece taken */
        var valid = Ksparov.board(Constants.selected_piece).move(x, y, Ksparov.board)
        if (valid) {
          /* the move being valid addit to the list of moves */
          Save.add_move2
          /* If the move is valid, apply the new board */
          DrawActions.draw_game_board(Ksparov.board)
          Constants.players(Constants.curr_player).moved = true
        } else {
          /* If the move is invalid, research for a first click */
          Constants.first_choice_done = false
          DrawActions.clear_possible_moves
        }
      }
    }
  }

  override def check_pat : Boolean = {
    var sum = 0
    for (i <- 0 to Ksparov.board.length / 2 - 1) {
      sum = sum + Ksparov.board(i + 16 * (1 - id)).possible_moves(Ksparov.board).length
    }
    if (sum == 0) {
      Constants.game_nulle = true
      true
    } else {
      false
    }
  }

  override def ai_promotion : Unit = {
    ()
  }
}

/* This object defines constans and not so constants variable that are used during the process. */
object Constants {

  var dim_small = new Dimension (70, 70)
  var dim_big = new Dimension (210, 70)
  var dim_message_drawer = new Dimension (350, 70)
  var dim_path = ""
  var resources_path = "src/main/resources/"

  var text_font = new Font ("Gill Sans Cyr MT", 1, 16)
  var num_dead_font = new Font("Arial", 0, 25)

  var resolution = java.awt.Toolkit.getDefaultToolkit().getScreenSize();

  def apply_resolution {

    /* Defining the dimension and numbers of the cases */
    resolution = java.awt.Toolkit.getDefaultToolkit().getScreenSize();

    if (resolution.getHeight < 1000.0) {
      dim_path = "Min/"
      dim_small = new Dimension (50, 50)
      dim_big = new Dimension (150, 50)
      dim_message_drawer = new Dimension (250, 50)
      text_font = new Font ("Gill Sans Cyr MT", 1, 11)
      num_dead_font = new Font("Arial", 0, 20)
    } else {
      dim_path = "Max/"
    }
  resources_path = "src/main/resources/" + dim_path
  }

  val nb_case_board = 8
  var nb_grid = 2

  /* Defining the path to find every resource used in the programm */
  var pieces_path = ""
  var save_path = "src/main/resources/Saves/"
  var small_texture_path = ""

  /* Defining the color of the text on the board. */
  var text_color = Color.black

  /* This is a regexpr pattern to select the first digit of a string. */
  val pattern = new Regex("\\d")

  /* This method read the parameters in src/main/resources/Parameters and update constant varible according to it. */
  var lines = new Array[String](3)
  def apply_parameters = {
    var i = 0
    /* Initializing lines with empty strings. */
    lines = Array ("", "", "")

    /* Reading each lines of the file. */
    for (line <- Source.fromFile("src/main/resources/Parameters").getLines) {
      lines (i) = line.toString
      i += 1
    }

    /* Updating variables. */
    pieces_path = "Pieces/" + lines(1) + "/"
    small_texture_path = "Texture_small_" + lines(2) + ".png"

    /* Defining the text color depending on the texture selected. */
    lines(2).toInt match {
      case 1 => text_color = Color.white
      case 2 => text_color = Color.white
      case 3 => text_color = Color.black
      case 4 => text_color = Color.black
      case 5 => text_color = Color.red
    }
  }

  /* This method write the new parameters in the Parameters file. */
  def write_parameters (piece : String, texture : String) {
    var writer = new PrintWriter(new File ("src/main/resources/Parameters"))
    writer.write ("Lines of this file : 1 - Pieces, 2 - Texture\n")
    writer.write (piece + "\n")
    writer.write (texture + "\n")
    writer.close
  }

  var selected_case = 0
  var selected_grid = 0
  var selected_piece = -1
  var promoted_piece = new Pawn (0, -1, -1, 0)
  var selected_promotion = ""

  /* The type of game chosen : 0 for Human vs Human, 1 for Hhuman vs AI and 2 for AI vs AI. */
  var game_type = 0
  var alice_chess = false

  /* Arrays for the game : one with the 63 cases, one which counts the dead pieces for each player. */
  var grids = new Array [Array[DrawBoard.Case]] (nb_grid)
  var dead_pieces = Array(new Array[Int](5), new Array[Int](5))
  var promotion_buttons = Array(new Array[DrawBoard.DeadCase](4), new Array[DrawBoard.DeadCase](4))

  /* Arrays for kings : because we need an access to them we should instentiate them, idem for players. */
  var kings = new Array[King](2)
  var players = new Array[Player](2)

  /* Move variables : who is the current player and has the first choice been done. */
  var curr_player = 1
  var first_choice_done = false

  /* Game variable : is the game won or nulle. */
  var game_nulle = false
  var game_won = false
  var promotion = false

  /* The message drawer is instantiated here so we can change its text in DrawActions.draw_messages. */
  var message_drawer = new DrawBoard.MessageDrawer ("")
}

/* The main object of the application. */
object Ksparov {

  Constants.apply_resolution
  /* Reading the parameters from the file. */
  Constants.apply_parameters

  DrawBoard.create_grid_dead

  /* Define the first frame, later the contents variables will be change to display other frame. */
  var frame = new MainFrame {
    title = "Ksparov"
    contents = new DrawMenu.Menu
    peer.setLocationRelativeTo(null)
  }

  /* The 2 * 16 pieces bord is created. */
  var board = new Array[Piece](32)

  /* Define the initial board, 0 for player black and 1 for white with the black player on the top of the chessboard. */
  def init_board {
    for (p <- 0 to 1) {
      for(i <- 0 to 7) {
        board((1 - p) * 16 + i) = new Pawn(p, i, 1 + (1 - p) * 5, 0)
      }
      board(8 + (1 - p) * 16) = new Rook(p, 0, (1 - p) * 7, 0)
      board(9 + (1 - p) * 16) = new Rook(p, 7, (1 - p) * 7, 0)
      board(10 + (1 - p) * 16) = new Knight(p, 1, (1 - p) * 7, 0)
      board(11 + (1 - p) * 16) = new Knight(p, 6, (1 - p) * 7, 0)
      board(12 + (1 - p) * 16) = new Bishop(p, 2, (1 - p) * 7, 0)
      board(13 + (1 - p) * 16) = new Bishop(p, 5, (1 - p) * 7, 0)
      board(14 + (1 - p) * 16) = Constants.kings(p)
      board(15 + (1 - p) * 16) = new Queen(p, 3, (1 - p) * 7, 0)
    }
  }

  /* Return the index in the game_board of the piece of position (x, y) */
  def get_piece_of_pos (x : Int, y : Int, grid_id : Int) {
    for (i <- 0 to 31){
      if (Ksparov.board(i).pos_x == x && Ksparov.board(i).pos_y == y && Ksparov.board(i).grid == grid_id){
        Constants.selected_piece = i
      }
    }
  }

  def promotion (p : Int) = {
    val pawn_index = Ksparov.board.indexOf(Constants.promoted_piece)  
    var new_piece = Constants.selected_promotion match {
      case "Knight" => new Knight (p, Constants.promoted_piece.pos_x, Constants.promoted_piece.pos_y, Constants.promoted_piece.grid)
      case "Bishop" => new Bishop (p, Constants.promoted_piece.pos_x, Constants.promoted_piece.pos_y, Constants.promoted_piece.grid)
      case "Rook" => new Rook (p, Constants.promoted_piece.pos_x, Constants.promoted_piece.pos_y, Constants.promoted_piece.grid)
      case "Queen" => new Queen (p, Constants.promoted_piece.pos_x, Constants.promoted_piece.pos_y, Constants.promoted_piece.grid)
    }
    board (pawn_index) = new_piece
    var king = Constants.kings(1 - p)
    if (Checkmate.move_is_possible (new_piece, king.pos_x, king.pos_y, board ) ) {
      king.attackers = king.attackers :+ new_piece
    }
    Save.add_prom_to_move(Constants.selected_promotion, !king.attackers.isEmpty)
    DrawActions.disable_promotion (p)

    check_game_status (Constants.curr_player)

    if (Constants.players(Constants.curr_player).ai && Constants.game_type == 2) {
      play_move (0, 0)
    }
  }

  def check_game_status (player : Int) {
  /* Check if there is a mate after the move. */
    if (Checkmate.check_mate (Ksparov.board, player)) {
     /* If so, finish the game. */
      DrawActions.draw_messages ("Mate", player)
      Constants.game_won = true
    } else {
      /* Check if there is pat. */
      if (Constants.players(player).check_pat) {
        DrawActions.draw_messages ("Pat", player)
      } else {
        /* Else check if there is check. */
        if (Constants.kings(player).attacked) {
          DrawActions.draw_messages ("Check", player)
        } else {
          /* Else, else, else ... Continue and give the hand to the other player. */
          DrawActions.draw_messages ("Current_turn", player)
        }
      }
    }
  }

  /* Called when click on a case of the board, defines the movment action. */
  def play_move(x : Int, y : Int) {
    /* Checking if the game has been won. */
    if (Constants.game_won || Constants.game_nulle || Constants.promotion) {
      /* If so, don't do anything, just wait for other button to be pressed. */
    } else {
      /* Else get the move, and if the player has moved, go on. */
      Constants.players(Constants.curr_player).getmove
      if (Constants.players(Constants.curr_player).moved) {
        Constants.players(Constants.curr_player).moved = false
        check_game_status (1 - Constants.curr_player)
        if (Constants.promotion) {
          DrawActions.draw_messages ("Promotion", Constants.curr_player)
        }
        Constants.curr_player = 1 - Constants.curr_player
        Constants.first_choice_done = false
      }
      /* If the next player is an IA and we are in Human vs AI, play the AI move in a row. */
      if (Constants.players(Constants.curr_player).ai && Constants.game_type == 2) {
        play_move (0, 0)
      }
    }
  }

  /* Define the variable for a new game, called after the game type selection. */
  def init_game (n : Int) {
    /* Instantiate the kings and then the new board. */
    Constants.kings = Array(new King (0, 4, 7, 0), new King (1, 4, 0, 0))
    DrawBoard.init_grids
    Ksparov.init_board
    DrawActions.draw_game_board(Ksparov.board)
    Save.init 
    /* Defines the welcome message and types of players depending on the game type chosen. */
    n match {
      case 1 =>
        Constants.players(0) = new Human(0)
        Constants.players(1) = new Human(1)
        Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 2 =>
        /* Draw a random number to know the color of each players. */
        if (scala.util.Random.nextInt(2) == 0) {
          Constants.players(0) = new Human(0)
          Constants.players(1) = new AI(1)
          Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov, vous jouez les noirs,<br>cliquez pour lancer la partie !</div></html>")
        } else {
          Constants.players(1) = new Human(1)
          Constants.players(0) = new AI(0)
          Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenu dans Ksparov, <br> vous jouez les blancs !</html>")
        }
      case 3 =>
        Constants.game_type = 2
        Constants.players(0) = new AI(0)
        Constants.players(1) = new Human(1)
        Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 4 => 
        Constants.game_type = 2
        Constants.players(0) = new Human(0)
        Constants.players(1) = new AI(1)
        Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov, vous jouez les noirs,<br>cliquez pour lancer la partie !</div></html>")
      case 5 =>
        Constants.players(1) = new AI(1)
        Constants.players(0) = new AI(0)
        Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode IA vs IA : <br>cliquez pour voir le prochain coup !</html>")
      case 6 =>
        Constants.players(1) = new Load.Reproducer(1)
        Constants.players(0) = new Load.Reproducer(0)
        Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode Spectateur : <br>cliquez pour voir le prochain coup !</html>")
    }
    /* Defines the game as not yet won, and the white player as the first player. */
    Constants.game_won = false
    Constants.game_nulle = false
    Constants.curr_player = 1
  }

  /* The Swing application with frame in it. */
  var application = new SimpleSwingApplication {
    def top = frame
  }

  /* Call the swing application to be launched. */
  def main(argv : Array[String]) {
    application.main(Array())
  }
}
