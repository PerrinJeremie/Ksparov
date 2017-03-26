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

/** Defines a player,then extended in Human just below and in AI in rules.scala. */
abstract class Player (n : Int) {
  /** The id of the player */
  val id : Int = n
  /** True if the player has moved */
  var moved = false
  /** ai true if the player is an Computer player or a Reproducer */
  var ai = false
  /** Method that apply the move chose by the player */
  def getmove : Unit
  /** Method that check if the player is pat or not */
  def check_pat : Boolean
  /** Returns true if  */
  def check_nulle : Boolean = {
    Ksparov.curr_game.game_nulle = Nulle.trivial_nulle (Ksparov.curr_game.board)
    Ksparov.curr_game.game_nulle
  }
  /** Return true if the game is nulle because there has been 50 boring move */
  def check_boring_game : Boolean = {
    Ksparov.curr_game.game_nulle = (Ksparov.curr_game.nb_boring_moves >= 50)
    Ksparov.curr_game.game_nulle
  }
  /** Return true if the game is nulle because there has been a triple repetition in position */
  def check_triple_repetition : Boolean = {
    Ksparov.curr_game.game_nulle = Aux.contains_triplicates(Ksparov.curr_game.hashed_positions)
    Ksparov.curr_game.game_nulle 
  }
  /** Method that apply the promotion for the AI or the Reproducer */
  def ai_promotion : Unit
  /** Defines the current time of the player */
  var actual_time = 0
  /** Defines the number of moves played by the player in the current period */
  var nb_move = 0
  /** Defines the current period of the player */
  var actual_period = 0
}

/** Class for a human player */
class Human(n : Int) extends Player(n : Int) {

  /** Returns true if the piece of the position given in argument is owned by the player. */
  def isHis(x : Int, y : Int, grid : Int) : Boolean = {
    /** True if every pieces has been checked */
    var done = false
    // We run from 0 to 15 for the player 1 (white) and from 16 to 31 for the player 0 (black)
    for (i <- (1 - id) * 16 to (2 - id) * 16 - 1) {
      done = done || ((Ksparov.curr_game.board(i).pos_x == x) && (Ksparov.curr_game.board(i).pos_y == y) && Ksparov.curr_game.board(i).grid == grid)
    }
    return done
  }

  /** Apply the move selected by the player */
  override def getmove : Unit = {
    /** The x axis position */
    var x = Ksparov.curr_game.selected_case % 8
    /** The y axis position */
    var y = Ksparov.curr_game.selected_case / 8
    /** The grid for the move */
    var grid = Ksparov.curr_game.selected_grid
    /** The current board of the game */
    var board = Ksparov.curr_game.board
    /** The selected piece */
    var piece = Ksparov.curr_game.selected_piece
    // First click, checking if the piece selected is own by the player.
    if (isHis(x, y, grid)) {
      // If so validate the first clic and open the second click. 
      Ksparov.curr_game.first_choice_done = true
      // Then select the piece of position and draw possible moves. 
      Ksparov.get_piece_of_pos(x, y, grid)
      piece = Ksparov.curr_game.selected_piece
      DrawActions.clear_possible_moves
      DrawActions.draw_possible_moves(board(piece).possible_moves(board), x, y, board(piece).grid)
    } else {
      /* Because we cannot move on a piece own by the player, we can enter in the else here.
         Second click, checking if the first has been done. */
      if (Ksparov.curr_game.first_choice_done) {
        /* loads part of the move to be added but has to wait for promotion information */
        Save.add_move1 (Ksparov.curr_game.selected_piece, (x,y))
        /* The variable valid check if the move is valid, and p is the optionnal piece taken */
        var valid = board(Ksparov.curr_game.selected_piece).move(x, y, board)
        if (valid) {
          /* the move being valid addit to the list of moves */
          Save.add_move2
          /* If the move is valid, apply the new board */
          DrawActions.draw_game_board(board)
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true
        } else {
          /* If the move is invalid, research for a first click */
          Ksparov.curr_game.first_choice_done = false
          DrawActions.clear_possible_moves
        }
      }
    }
  }

  /** Returns true if the human player is in pat */
  override def check_pat : Boolean = {
    /** Number of moves possible for a player */
    var sum = 0
    for (i <- 0 to Ksparov.curr_game.board.length / 2 - 1) {
      sum = sum + Ksparov.curr_game.board(i + 16 * (1 - id)).possible_moves(Ksparov.curr_game.board).length
    }
    // If there is no move possible, the player is in pat
    if (sum == 0) {
      Ksparov.curr_game.game_nulle = true
      true
    } else {
      false
    }
  }

  /** There is no such method for a non-ai player */
  override def ai_promotion : Unit = {
    ()
  }
}

/** Defines every thing for the time in the game : game clock... */
object Time {
  /** Current second time of the computer */
  var new_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
  /** Second time of the computer of the last measure */
  var last_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)

  /** Defines a period of game */
  class Period (period_time : Int, period_move : Int, increment : Int) {
    /** The time for the period */
    var time = period_time
    /** The minimum number of moves that should be played in the period */
    var nb_move = period_move
    /** The increment after each move for this period */
    var inc = increment
  }

  /** Add 0 on the left of a number to have a string with exactly 2 digits */
  def convert_in_two_digit (num : Int) = {
    if (num <= 0) {
        "00"
    } else {
      if (num < 10) {
        "0" + num.toString
      } else {
        num.toString
      }
    }
  }

  /** Return the string of a format hh:mm:ss from a time given in second */
  def hhmmss_to_int (time : String) = {
    /** Number of hours in the time */
    var hour = time.substring(0, 2)
    /** Number of minutes in the time */
    var min = time.substring(3, 5)
    /** Number of seconds in the time */
    var sec = time.substring(6, 8)
    sec.toInt + 60 * min.toInt + 3600 * hour.toInt
  }

  /** Return the number of second in a time given in the format hh:mm:ss */
  def int_to_hhmmss (time : Int) = {
    /** Number of hours in the time */
    var hour = time / 3600
    /** Number of minutes in the time */
    var min = time / 60 - hour * 60
    /** Number of seconds in the time */
    var sec = time % 60
    convert_in_two_digit (hour) + ":" + convert_in_two_digit (min) + ":" + convert_in_two_digit (sec)
  }

  /** Defines the thread used to display the current clock during a game, and the board */
  class TimeThread extends Thread {
    /** Display the board every 200 milliseconds */
    override def run {
      // The thread runs as long as we do not stop it or the game is not over 
      while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
        // The display is actualize every 200 milliseconds
        Thread.sleep (200)
        // If after its sleep the thread is still alive, we update the current date
        if (Ksparov.curr_game.thread_in_life) {
          Time.new_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
          // If the current date is not the same as the previous one, we adapt the display
          if (Time.new_time != Time.last_time) {
            var player = Ksparov.curr_game.players(Ksparov.curr_game.curr_player)
            player.actual_time -= 1
            var dimension = Ksparov.frame.bounds.getSize()
            Ksparov.frame.contents = new DrawBoard.Board
            Ksparov.frame.size = dimension
            Time.last_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
            if (player.actual_time <= 0 && Time.clock_available) {
              if (player.nb_move < Time.periods(player.actual_period).nb_move || player.actual_period + 1 == Time.periods.length) {
                DrawActions.clear_possible_moves
                Ksparov.curr_game.game_won = true
                DrawActions.draw_game_messages ("Time", 1 - Ksparov.curr_game.curr_player)
              } else {
               player.actual_period += 1
                player.actual_time = Time.periods(player.actual_period).time
                player.nb_move = 0
              }
            }
          } else {
            var dimension = Ksparov.frame.bounds.getSize()
            Ksparov.frame.contents = new DrawBoard.Board
            Ksparov.frame.size = dimension
          }
        }
      }
    }
  }
  var nb_period = 2
  var periods = new Array [Time.Period] (nb_period) 
  var clock_available = true
}

/* The main object of the application. */
object Ksparov {

  class Game (type_id : Int, number_grid : Int, alice : Boolean) {
  
  var nb_grid = number_grid
  var ai_turn = false
  var thread_in_life = true
  var nb_boring_moves = 0

  var selected_case = 0
  var selected_grid = 0
  var selected_piece = 0
  var promoted_piece = new Pawn (0, 0, 0, 0)
  var selected_promotion = ""

  /* The type of game chosen : 0 for Human vs Human, 1 for Human vs AI and 2 for AI vs AI. */
  var game_type = type_id
  var alice_chess = alice

  /* Arrays for the game : one with the 63 cases, one which counts the dead pieces for each player. */
  var grids = new Array [Array[DrawBoard.Case]] (nb_grid)
  var dead_pieces = Array(new Array[Int](5), new Array[Int](5))
  var promotion_buttons = Array(new Array[DrawBoard.DeadCase](4), new Array[DrawBoard.DeadCase](4))
  var play_buttons = new Array [DrawBoard.PlayButton] (2)
  var board = new Array[Piece](32)

  /* Arrays for kings : because we need an access to them we should instentiate them, idem for players. */
  var kings = new Array[King](2)
  var players = new Array[Player](2)

  /* Move variables : who is the current player and has the first choice been done. */
  var curr_player = 0
  var first_choice_done = false

  /* Game variable : is the game won or nulle. */
  var game_nulle = false
  var game_won = false
  var promotion = false
  var hashed_positions : List[Int] = List()

  /* The message drawer is instantiated here so we can change its text in DrawActions.draw_messages. */
  var message_drawer = new DrawBoard.MessageDrawer ("")
  var timer = new Time.TimeThread
  var ai_move = new AIMoveThread
}

def init_board {
    for (p <- 0 to 1) {
      for(i <- 0 to 7) {
        Ksparov.curr_game.board((1 - p) * 16 + i) = new Pawn(p, i, 1 + (1 - p) * 5, 0)
      }
      Ksparov.curr_game.board(8 + (1 - p) * 16) = new Rook(p, 0, (1 - p) * 7, 0)
      Ksparov.curr_game.board(9 + (1 - p) * 16) = new Rook(p, 7, (1 - p) * 7, 0)
      Ksparov.curr_game.board(10 + (1 - p) * 16) = new Knight(p, 1, (1 - p) * 7, 0)
      Ksparov.curr_game.board(11 + (1 - p) * 16) = new Knight(p, 6, (1 - p) * 7, 0)
      Ksparov.curr_game.board(12 + (1 - p) * 16) = new Bishop(p, 2, (1 - p) * 7, 0)
      Ksparov.curr_game.board(13 + (1 - p) * 16) = new Bishop(p, 5, (1 - p) * 7, 0)
      Ksparov.curr_game.board(14 + (1 - p) * 16) = Ksparov.curr_game.kings(p)
      Ksparov.curr_game.board(15 + (1 - p) * 16) = new Queen(p, 3, (1 - p) * 7, 0)
    }
  }

  /* Return the index in the game_board of the piece of position (x, y) */
  def get_piece_of_pos (x : Int, y : Int, grid_id : Int) {
    for (i <- 0 to 31){
      if (Ksparov.curr_game.board(i).pos_x == x && Ksparov.curr_game.board(i).pos_y == y && Ksparov.curr_game.board(i).grid == grid_id){
        Ksparov.curr_game.selected_piece = i
      }
    }
  }

  def promotion (p : Int) = {
    val pawn_index = Ksparov.curr_game.board.indexOf(Ksparov.curr_game.promoted_piece)
    var new_piece = Ksparov.curr_game.selected_promotion match {
      case "Knight" => new Knight (p, Ksparov.curr_game.promoted_piece.pos_x, Ksparov.curr_game.promoted_piece.pos_y, Ksparov.curr_game.promoted_piece.grid)
      case "Bishop" => new Bishop (p, Ksparov.curr_game.promoted_piece.pos_x, Ksparov.curr_game.promoted_piece.pos_y, Ksparov.curr_game.promoted_piece.grid)
      case "Rook" => new Rook (p, Ksparov.curr_game.promoted_piece.pos_x, Ksparov.curr_game.promoted_piece.pos_y, Ksparov.curr_game.promoted_piece.grid)
      case "Queen" => new Queen (p, Ksparov.curr_game.promoted_piece.pos_x, Ksparov.curr_game.promoted_piece.pos_y, Ksparov.curr_game.promoted_piece.grid)
    }
    Ksparov.curr_game.board (pawn_index) = new_piece
    var king = Ksparov.curr_game.kings(1 - p)
    if (Checkmate.move_is_possible (new_piece, king.pos_x, king.pos_y, Ksparov.curr_game.board)) {
      king.attackers = king.attackers :+ new_piece
    }
    Save.add_prom_to_move(Ksparov.curr_game.selected_promotion, !king.attackers.isEmpty)
    DrawActions.disable_promotion (p)

    if ( !Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai){
      Ksparov.curr_game.curr_player = 1 - Ksparov.curr_game.curr_player
    }

    check_game_status (Ksparov.curr_game.curr_player)

    if ((Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai && Ksparov.curr_game.game_type == 2) || (Ksparov.curr_game.game_type == 6 && !Ksparov.curr_game.players(1 - Ksparov.curr_game.curr_player).ai)) {
      Ksparov.curr_game.ai_turn = true
    }
  }

  def check_game_status (player : Int) {
  /* Check if there is a mate after the move. */
    if (Checkmate.check_mate (Ksparov.curr_game.board, player)) {
     /* If so, finish the game. */
      DrawActions.draw_game_messages ("Mate", player)
      Save.whowins = player
      Ksparov.curr_game.game_won = true
    } else {
      /* Check if there is pat. */
      if (Ksparov.curr_game.players(player).check_pat) {
        Save.whowins = -1
        DrawActions.draw_game_messages ("Pat", player)
      } else {
      /*Check if the game is nulle */
        if (Ksparov.curr_game.players(player).check_nulle) {
          Save.whowins = -1
          DrawActions.draw_game_messages ("Nulle", player)
        }
        else {
      /*Check if players are not asleep because nothing has been happening for 50 moves */
          if (Ksparov.curr_game.players(player).check_boring_game) {
            Save.whowins = -1
            DrawActions.draw_game_messages ("50coups", player)
          }
          else {
            if (Ksparov.curr_game.players(player).check_triple_repetition) {
              Save.whowins = -1
              DrawActions.draw_game_messages ("TripleRepetition", player)
            }
            else {
        /* Else check if there is check. */
        if (Ksparov.curr_game.kings(player).attacked) {
          DrawActions.draw_game_messages ("Check", player)
        } else {
          if (Load.specialmessage != ""){
            DrawActions.draw_game_messages (Load.specialmessage,player)
            Load.specialmessage = ""
          } else {
            if (Load.finalresult != ""){
              DrawActions.draw_game_messages (Load.finalresult,player)
              Load.finalresult = ""
            }
            else{
              /* Else, else, else ... Continue and give the hand to the other player. */
              DrawActions.draw_game_messages ("Current_turn", player)
            }
          }
        }
      }
    }
  }
}
}
}
  /* Called when click on a case of the board, defines the movment action. */
  def play_move {
    /* Checking if the game has been won. */
    if (Ksparov.curr_game.game_won || Ksparov.curr_game.game_nulle || Ksparov.curr_game.promotion) {
      /* If so, don't do anything, just wait for other button to be pressed. */
    } else {
      /* Else get the move, and if the player has moved, go on. */
      Ksparov.curr_game.players(Ksparov.curr_game.curr_player).getmove
      if (Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved) {
        Ksparov.curr_game.players(Ksparov.curr_game.curr_player).nb_move += 1
        if (Time.clock_available) {
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).actual_time += Time.periods(Ksparov.curr_game.players(Ksparov.curr_game.curr_player).actual_period).inc
        }
        Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = false
        Ksparov.curr_game.hashed_positions = (Aux.array_to_hashed_string(Ksparov.curr_game.board)) :: Ksparov.curr_game.hashed_positions
        check_game_status (1 - Ksparov.curr_game.curr_player)
        if (Ksparov.curr_game.promotion) {
          DrawActions.draw_game_messages ("Promotion", Ksparov.curr_game.curr_player)
        } else {
          Ksparov.curr_game.curr_player = 1 - Ksparov.curr_game.curr_player
        }
        Ksparov.curr_game.first_choice_done = false
      }
      /* If the next player is an IA and we are in Human vs AI, play the AI move in a row. */
      if ((Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai && Ksparov.curr_game.game_type == 2) || (Ksparov.curr_game.game_type == 6 && !Ksparov.curr_game.players(1 - Ksparov.curr_game.curr_player).ai)) {
        Ksparov.curr_game.ai_turn = true
      }
    }
  }

  /* Define the variable for a new game, called after the game type selection. */
  def init_game (n : Int) {
    if (Time.periods.length < 1) {
      Time.clock_available = false
    } else {
      Time.clock_available = true
    }
    /* Instantiate the kings and then the new board. */
    Ksparov.curr_game.kings = Array(new King (0, 4, 7, 0), new King (1, 4, 0, 0))
    Ksparov.curr_game.play_buttons = Array (new DrawBoard.PlayButton (0), new DrawBoard.PlayButton (1))
    Ksparov.curr_game.hashed_positions = List()
    Ksparov.curr_game.nb_boring_moves = 0
    DrawBoard.init_grids
    DrawBoard.create_grid_dead
    Ksparov.init_board
    DrawActions.draw_game_board(Ksparov.curr_game.board)
    Save.init
    /* Defines the welcome message and types of players depending on the game type chosen. */
    n match {
      case 1 =>
        Ksparov.curr_game.players(0) = new Human(0)
        Ksparov.curr_game.players(1) = new Human(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 2 =>
        /* Draw a random number to know the color of each players. */
        if (scala.util.Random.nextInt(2) == 0) {
          Ksparov.curr_game.players(0) = new Human(0)
          Ksparov.curr_game.players(1) = new AI(1)
          Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov, vous jouez les noirs,<br>cliquez pour lancer la partie !</div></html>")
        } else {
          Ksparov.curr_game.players(1) = new Human(1)
          Ksparov.curr_game.players(0) = new AI(0)
          Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenu dans Ksparov, <br> vous jouez les blancs !</html>")
        }
      case 3 =>
        Ksparov.curr_game.game_type = 2
        Ksparov.curr_game.players(0) = new AI(0)
        Ksparov.curr_game.players(1) = new Human(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 4 =>
        Ksparov.curr_game.game_type = 2
        Ksparov.curr_game.players(0) = new Human(0)
        Ksparov.curr_game.players(1) = new AI(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov, vous jouez les noirs,<br>cliquez pour lancer la partie !</div></html>")
      case 5 =>
        Ksparov.curr_game.players(1) = new AI(1)
        Ksparov.curr_game.players(0) = new AI(0)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode IA vs IA : <br>cliquez pour voir le prochain coup !</html>")
      case 6 =>
        Time.clock_available = false
        Ksparov.curr_game.players(1) = new Load.Reproducer(1)
        Ksparov.curr_game.players(0) = new Load.Reproducer(0)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode Spectateur : <br>cliquez pour voir le premier coup !</html>")
    }
    /* Defines the game as not yet won, and the white player as the first player. */
    Ksparov.curr_game.game_won = false
    Ksparov.curr_game.game_nulle = false
    Ksparov.curr_game.curr_player = 1
    Ksparov.curr_game.play_buttons = Array (new DrawBoard.PlayButton (0), new DrawBoard.PlayButton (1))

    if (Time.clock_available) {
      Ksparov.curr_game.players(0).actual_time = Time.periods(0).time
      Ksparov.curr_game.players(1).actual_time = Time.periods(0).time
    }

    Ksparov.curr_game.thread_in_life = true
    Ksparov.curr_game.timer.start
    Ksparov.curr_game.ai_move.start
    Time.last_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
  }

  var curr_game = new Game (0, 0, false)

  Display.apply_resolution
  Parameters.apply

  var frame = new MainFrame {
    title = "Ksparov"
    contents = new DrawMenu.Menu
    peer.setLocationRelativeTo(null)
  }

  var application = new SimpleSwingApplication {
    def top = frame
  }

  /* Call the swing application to be launched. */
  def main(argv : Array[String]) {
    application.main(Array())
  }
}
