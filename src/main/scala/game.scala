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

/** Defines a player,then extended in Human just below and in AI in rules.scala. 
*
* @param n The id of the player 
*/
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
  /** Returns true if players do not have enough pieces to mate */
  def check_trivial_nulle : Boolean = {
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
  /** Defines the amount of time the player have for its current period */
  var actual_time = 0
  /** Defines the number of moves played by the player in the current period */
  var nb_move = 0
  /** Defines the current period of the player */
  var actual_period = 0
}

/** Class for a human player */
class Human(n : Int) extends Player(n : Int) {

  /** Returns true if the piece of the position given in argument is owned by the player. 
  *
  * @param x The x position of the piece
  * @param y The y position of the piece 
  * @param grid The grid id of the piece
  * @return True if the piece is owned by the player
  */
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

  /** Defines a period of game 
  *
  * @param period_time The duration of the period
  * @param period_move The minimum amount of moves the player should have done by the end of the period
  * @param increment The increment of the period, 0 if there is no 
  */
  class Period (period_time : Int, period_move : Int, increment : Int) {
    /** The time for the period */
    var time = period_time
    /** The minimum number of moves that should be played in the period */
    var nb_move = period_move
    /** The increment after each move for this period */
    var inc = increment
  }

  /** Add 0 on the left of a number to have a string with exactly 2 digits *
  *
  * @param num The number to convert
  * @return The string with the 2 digits number with 0 on the left is needed
  */
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

  /** Return the string of a format hh:mm:ss from a time given in second 
  *
  * @param time The time to be converted
  * @return The number of second in the time given
  */
  def hhmmss_to_int (time : String) = {
    /** Number of hours in the time */
    var hour = time.substring(0, 2)
    /** Number of minutes in the time */
    var min = time.substring(3, 5)
    /** Number of seconds in the time */
    var sec = time.substring(6, 8)
    sec.toInt + 60 * min.toInt + 3600 * hour.toInt
  }

  /** Return the number of second in a time given in the format hh:mm:ss 
  *
  * @param time The number of second to be converted
  * @return The string of the time in format hh:mm:ss
  */
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
          // If the current date is not the same as the previous one, we adapt the display to display the new second
          if (Time.new_time != Time.last_time) {
            /** The actual player of the game */
            var player = Ksparov.curr_game.players(Ksparov.curr_game.curr_player)
            // We reduce the time left to to the player for the period of one second
            player.actual_time -= 1
            /** The dimension of the current frame */
            var dimension = Ksparov.frame.bounds.getSize()
            if (Time.clock_available){
              Ksparov.curr_game.clock_array(1).change_time
              Ksparov.curr_game.clock_array(0).change_time
            }
            // We keep the previous size, so the user can modify it 
            Ksparov.frame.size = dimension
            Time.last_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
            // If the current player is out of time for the priod
            if (player.actual_time <= 0 && Time.clock_available) {
              // If he has not play enough movement or if he is in the last period, the player lose the game due to the clock
              if (player.nb_move < Time.periods(player.actual_period).nb_move || player.actual_period + 1 == Time.periods.length) {
                DrawActions.clear_possible_moves
                Ksparov.curr_game.game_won = true
                DrawActions.draw_game_messages ("Time", 1 - Ksparov.curr_game.curr_player)
              } else {
                // Else, we switch to the next period 
                player.actual_period += 1
                player.actual_time = Time.periods(player.actual_period).time
                player.nb_move = 0
              }
            }
          } 
        }
      }
    }
  }

  /** Number of periods of the game */
  var nb_period = 2
  /** Array of all periods of the game */
  var periods = new Array [Time.Period] (nb_period) 
  /** True if clocks are enabled */
  var clock_available = true
}

/** The main application of Ksparov */
object Ksparov {

  /** Defines all the variables of a game 
  *
  * @param type_id The game type 
  * @param number_grid The number of grids used for the game
  * @param alice True if the game is an Alice one 
  */
  class Game (type_id : Int, number_grid : Int, alice : Boolean) {

  /** Direct access to the clock components */
  var clock_array = Array (new DrawBoard.Clock (0), new DrawBoard.Clock (1))

  /** The number of grid used to play the game */
  var nb_grid = number_grid
  /** True if the turn is for an AI, used to wake up the AIMoveThread */
  var ai_turn = false
  /** True if threads should be alive, when passed at false, AiMoveThread and TimeThread stop */
  var thread_in_life = true
  /** Count the number of boring moves played, if the amount reach 50, the game is nulle */
  var nb_boring_moves = 0

  /** The case selected by the player by a click on a case */
  var selected_case = 0
  /** The grid on which the player has clicked */
  var selected_grid = 0
  /** The index in the array of piece of the piece selected by the player */
  var selected_piece = 0
  /** The piece selected for the promotion of a pawn */
  var promoted_piece = new Pawn (0, 0, 0, 0)
  /** The name of the piece selected for the promotion */
  var selected_promotion = ""

  /** Type of game chosen : 0 for Human vs Human, 1 for Human vs AI and 2 for AI vs AI... */
  var game_type = type_id
  /** True is the game is an Alice one */
  var alice_chess = alice

  /** Array of the grids of the game */
  var grids = new Array [Array[DrawBoard.Case]] (nb_grid)
  /** Array that counts the number of dead pieces for each player */
  var dead_pieces = Array(new Array[Int](5), new Array[Int](5))
  /** Array for the dead piece icon, they are enabled for the promotion to select the piece */
  var promotion_buttons = Array(new Array[DrawBoard.DeadCase](4), new Array[DrawBoard.DeadCase](4))
  /** Array of the 2 butons to start to play in a loaded game */
  var play_buttons = new Array [DrawBoard.PlayButton] (4)
  /** Array of the 32 pieces of the game */
  var board = new Array[Piece](32)

  /** Array of the two kings of the game */
  var kings = new Array[King](2)
  /** Array of the two players of the game */
  var players = new Array[Player](2)

  /** The current player */
  var curr_player = 0
  /** True if the human player has done his first move, the selection of a piece he owns */
  var first_choice_done = false

  /** True if the game is over because of a nulle status */
  var game_nulle = false
  /** True if the game is over because of a player has won it */
  var game_won = false
  /** True if there is a promotion ongoing */
  var promotion = false
  /** The list of the hashed of the board, it records every board of the game to check the triple repetition */
  var hashed_positions : List[Int] = List()

  /** The message drawer that is on the bottom of the screen */
  var message_drawer = new DrawBoard.MessageDrawer ("")

  /** The thread to display the time and the board repetidly */
  var timer = new Time.TimeThread
  /** The thread that get the AI move */
  var ai_move = new AIMoveThread
}

  /** Set Ksparov.curr_game.selected_piece to the index in the game_board of the given grid of the piece of position passed in argument 
  *
  * @param x The x coordinate of the piece
  * @param y The y coordinate of the piece
  * @param grid_id The grid_id coordinate of the piece
  */
  def get_piece_of_pos (x : Int, y : Int, grid_id : Int) {
    for (i <- 0 to 31) {
      if (Ksparov.curr_game.board(i).pos_x == x && Ksparov.curr_game.board(i).pos_y == y && Ksparov.curr_game.board(i).grid == grid_id){
        Ksparov.curr_game.selected_piece = i
      }
    }
  }

  /** Apply the promotion with the selected piece *
  *
  * @param p The player the promotion is for
  */
  def promotion (p : Int) = {
    /** The index of the pawn that is promoted */
    val pawn_index = Ksparov.curr_game.board.indexOf(Ksparov.curr_game.promoted_piece)
    /** The promoted pawn */
    var pawn = Ksparov.curr_game.promoted_piece
    /** The new piece that will replace the pawn */
    var new_piece = Ksparov.curr_game.selected_promotion match {
      case "Knight" => new Knight (p, pawn.pos_x, pawn.pos_y, pawn.grid)
      case "Bishop" => new Bishop (p, pawn.pos_x, pawn.pos_y, pawn.grid)
      case "Rook" => new Rook (p, pawn.pos_x, pawn.pos_y, pawn.grid)
      case "Queen" => new Queen (p, pawn.pos_x, pawn.pos_y, pawn.grid)
    }
    // We set the promotion
    Ksparov.curr_game.board (pawn_index) = new_piece
    // We check if after the promotion there no mate 
    var king = Ksparov.curr_game.kings(1 - p)
    if (Checkmate.move_is_possible (new_piece, king.pos_x, king.pos_y, Ksparov.curr_game.board)) {
      king.attackers = king.attackers :+ new_piece
    }
    Save.add_prom_to_move(Ksparov.curr_game.selected_promotion, !king.attackers.isEmpty)
    // We disable the promotion buttons
    if (! Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai){
      DrawActions.disable_promotion (p)
    }

    // If the current player is not an ai, we switch to the next player because it has not been done since, for the ai it is different
    if (!Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai) {
      Ksparov.curr_game.curr_player = 1 - Ksparov.curr_game.curr_player
    }

    /** The current player */
    var player = Ksparov.curr_game.curr_player
    /** The current game type */
    var game_type = Ksparov.curr_game.game_type

    check_game_status (player)

    // If we are in a Human vs ai and it is the turn of the ai, we launch it
    if ((Ksparov.curr_game.players(player).ai && game_type == 2)) {
      Ksparov.curr_game.ai_turn = true
    }
  }

  /** Check every possible status of the game and adpat game variables to the new status *
  *
  * @param player The player we will check the status
  */
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
        if (Ksparov.curr_game.players(player).check_trivial_nulle) {
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
            // Check if there has been 3 repitition of positions 
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
                  } else {
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

  /** Apply all the steps of a movement */
  def play_move {
    /** The current player */
    var player = Ksparov.curr_game.players(Ksparov.curr_game.curr_player)
    /** The increment of the current period */
    var increment = (
      if (Time.clock_available) {
        Time.periods(Ksparov.curr_game.players(Ksparov.curr_game.curr_player).actual_period).inc
      } else { 0 }
    )
    // Checking if the game has been won.
    if (Ksparov.curr_game.game_won || Ksparov.curr_game.game_nulle || Ksparov.curr_game.promotion) {
      // If so, don't do anything, just wait for other button to be pressed.
    } else {
      // Else get the move, and if the player has moved, go on.
      player.getmove
      if (player.moved) {
        // If the clock is enabled, adjust the number of moves of this period and the current time left of the player
        if (Time.clock_available) {
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).nb_move += 1
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).actual_time += increment
        }
        Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = false
        // We add the board to the hashed position list 
        Ksparov.curr_game.hashed_positions = (Aux.array_to_hashed_string(Ksparov.curr_game.board)) :: Ksparov.curr_game.hashed_positions
        // We check the status after the move 
        check_game_status (1 - Ksparov.curr_game.curr_player)
        // If there is a promotion after the move, we enable it, else we continue the game 
        if (Ksparov.curr_game.promotion) {
          DrawActions.draw_game_messages ("Promotion", Ksparov.curr_game.curr_player)
        } else {
          Ksparov.curr_game.curr_player = 1 - Ksparov.curr_game.curr_player
        }
        // Reinitializes the choice for the player 
        Ksparov.curr_game.first_choice_done = false
      }
      // If the next player is an IA and we are in Human vs AI, play the AI move in a row.
      if ((Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai && Ksparov.curr_game.game_type == 2) ) {
        Ksparov.curr_game.ai_turn = true
      }
    }
    if ( !(Ksparov.curr_game.game_type == 7) || Load.list_of_moves.length <= 1 || player.moved){
      Ksparov.frame.contents = new DrawBoard.Board {
        preferredSize = Ksparov.frame.contents(0).bounds.getSize()
      }
    } else {
      play_move
    }
  }


  /** Initializes the game after the game type selection 
  *
  * @param n The game type selected by the user
  */
  def init_game (n : Int) {
    // If there is no periods, we disable clocks
    if (Time.periods.length < 1) {
      Time.clock_available = false
    } else {
      Time.clock_available = true
    }
    // Instantiates the kings, hashed position...
    Ksparov.curr_game.kings = Array(new King (0, 4, 7, 0), new King (1, 4, 0, 0))
    Ksparov.curr_game.hashed_positions = List()
    Ksparov.curr_game.nb_boring_moves = 0

    // Initializes grids for hte game
    DrawBoard.init_grids
    DrawBoard.create_grid_dead

    //Initializes the board and draw it 
    BoardsT.init_alt_board("legal")
    DrawActions.draw_game_board(Ksparov.curr_game.board)

    //Initializes the play buttons 
      Ksparov.curr_game.play_buttons = Array (new DrawBoard.PlayButton (0, "h"), new DrawBoard.PlayButton (0, "ai"), new DrawBoard.PlayButton (1, "h"), new DrawBoard.PlayButton (1, "ai"))   

    // Saves the initialisation
    Save.init

    // Defines players and welcome message based on the game type
    n match {
      case 1 =>
        Ksparov.curr_game.players(0) = new Human(0)
        Ksparov.curr_game.players(1) = new Human(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 2 =>
        // Case of random colors for the human player in a game Human vs AI
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
        Ksparov.curr_game.players(0) = new AI2(0)
        Ksparov.curr_game.players(1) = new Human(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov,<br> les blancs commençent la partie !</html>")
      case 4 =>
        Ksparov.curr_game.game_type = 2
        Ksparov.curr_game.players(0) = new Human(0)
        Ksparov.curr_game.players(1) = new AI2(1)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Bienvenue dans Ksparov, vous jouez les noirs,<br>cliquez pour lancer la partie !</div></html>")
      case 5 =>
        Ksparov.curr_game.players(1) = new AI2(1)
        Ksparov.curr_game.players(0) = new AI(0)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode IA vs IA : <br>cliquez pour voir le prochain coup !</html>")
      case 6 =>
        Time.clock_available = false
        Ksparov.curr_game.players(1) = new Load.Reproducer(1)
        Ksparov.curr_game.players(0) = new Load.Reproducer(0)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode Spectateur : <br>cliquez pour voir le premier coup !</html>")
        case 7 =>
        Time.clock_available = false
        Ksparov.curr_game.players(1) = new Load.Reproducer(1)
        Ksparov.curr_game.players(0) = new Load.Reproducer(0)
        Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Mode Spectateur : <br>cliquez pour voir le premier coup !</html>")
    }

    // Defines the game as not yet won, and the white player as the first player
    Ksparov.curr_game.game_won = false
    Ksparov.curr_game.game_nulle = false
    Ksparov.curr_game.curr_player = 1

    // If clocks are enabled, initializes time for each player 
    if (Time.clock_available) {
      Ksparov.curr_game.players(0).actual_time = Time.periods(0).time
      Ksparov.curr_game.players(1).actual_time = Time.periods(0).time
    }

    // Start all threads 
    Ksparov.curr_game.thread_in_life = true
    Ksparov.curr_game.timer.start
    if (n != 1) {
      Ksparov.curr_game.ai_move.start
    }

    // Defines the start time of the game 
    Time.last_time = new java.text.SimpleDateFormat("ss").format(java.util.Calendar.getInstance().getTime)
  }

  /** The current played game */
  var curr_game = new Game (0, 0, false)

  // On the begining adapt the display to the resolution and apply Parameters from the Parameters file 
  Display.apply_resolution
  Parameters.apply

  /** The frame of the application */
  var frame = new MainFrame {
    title = "Ksparov"
    contents = new DrawMenu.Menu
    peer.setLocationRelativeTo(null)
  }

  /** The application Ksparov */
  var application = new SimpleSwingApplication {
    def top = frame
  }

  /** Call the swing application to be launched. 
  *
  * @param argv The array of strings of the argument passed to the program
  */
  def main (argv : Array[String]) {
    application.main(Array())
    var chooser = new javax.swing.JFileChooser()
    var file = chooser.getSelectedFile
  }
}
