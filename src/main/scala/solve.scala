/** The class for AI players 
*
* @param player The id of the player
*/
class AI (player : Int) extends Player (player) {

  /** True if the ai is in pat */
  var pat = false

	ai = true
  /** Array of boolean, true if the piece has already been checked for a move */
  var already_check = new Array[Boolean](16)

  override def getmove {
    // All piece are initially possible, so the array is false, but if a piece is dead (x negative), it should not be tried to move
    for (i <- 0 to 15) {
      if (Ksparov.curr_game.board((1 - id) * 16 + i).pos_x < 0) {
        already_check (i) = true
      } else {
        already_check (i) = false
      }
    }

    /** The random number for move choice */
    val r = scala.util.Random
    /** True while no move was played */
    var notdone = true

    // While no movement has been done and there are still pieces to try, search for a piece with a possible movement.
    while (notdone) {
      /** The random piece seleted */
      var ind = r.nextInt (16)
      // If the piece has not been tried, let's do it
      if (!already_check (ind)) {
        already_check (ind) = true
        /** The list of possible moves for the random piece */
        var t = Ksparov.curr_game.board ((1 - id) * 16 + ind).possible_moves (Ksparov.curr_game.board)
        // If there are possible moves, select one of them and apply it
        if (t.nonEmpty) {
          notdone = false
          var (i,j) = t(r.nextInt(t.size))
          // Save move
          Save.add_move1((1 - id) * 16 + ind, (i,j))
          if (Ksparov.curr_game.game_type > 7) {
            Ksparov.curr_game.write_to_the_pipe += (Ksparov.curr_game.board((1-id) * 16 + ind).pos_x + 97).toChar + (Ksparov.curr_game.board((1-id) * 16 + ind).pos_y + 1).toString
          }
          // Play move 
          Ksparov.curr_game.board((1 - id) * 16 + ind).move(i,j,Ksparov.curr_game.board)
          if (Ksparov.curr_game.game_type > 7) {
            Ksparov.curr_game.write_to_the_pipe += (Ksparov.curr_game.board((1-id) * 16 + ind).pos_x + 97).toChar + (Ksparov.curr_game.board((1-id) * 16 + ind).pos_y + 1).toString + "\n"
            Ksparov.curr_game.something_to_send = true
          }
        }
        // If no move was played and there is no more pieces left, the IA cannot move, thus the game is nulle
        if (!((already_check.find (p => p == false)).nonEmpty) && notdone) {
          notdone = false
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).asInstanceOf[AI].pat = true
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).asInstanceOf[AI].moved = true
          Ksparov.curr_game.game_nulle = true
          Ksparov.curr_game.curr_player = 1 - Ksparov.curr_game.curr_player
        }        
      }
    }
    Save.add_move2
    // Draw the new board
    DrawActions.draw_game_board(Ksparov.curr_game.board)
    Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true
  }

  // The check_pat here is only the value of the Boolean pat
  override def check_pat : Boolean = {
    pat
  }

  /** The promotion method for an ai player */
  def ai_promotion {
    /** The random piece for the promotion choice */
    val rand = scala.util.Random
    rand.nextInt(4) match {
      case 0 => Ksparov.curr_game.selected_promotion = "Queen"
      case 1 => Ksparov.curr_game.selected_promotion = "Bishop"
      case 2 => Ksparov.curr_game.selected_promotion = "Knight"
      case 3 => Ksparov.curr_game.selected_promotion = "Rook"
    }
    Ksparov.promotion (Ksparov.curr_game.curr_player)
  }
}

/** Thread to make the ai move, wait a certain time before the move */
class AIMoveThread extends Thread {

  def wait_time = {
    if (Parameters.ai_speed < 3) {
      800
    } else {
      if (Parameters.ai_speed == 4) {
        500
      } else {
        0
      }
    }
  }

  override def run {
    // While threads should be alive
    while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
      Thread.sleep(100)
      // If it is our turn, we move a piece
      if (Ksparov.curr_game.ai_turn) {
        Thread.sleep(wait_time)
        Ksparov.play_move
        Ksparov.curr_game.ai_turn = false
      }
      if (Ksparov.curr_game.game_type == 5) {
        Ksparov.curr_game.ai_turn = true
      }
    }
  }
}

class AI2 (player : Int) extends Player (player) {

  /** True if the ai is in pat */
  var pat = false

  ai = true

  override def getmove {

    for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
	  for (i <- 0 to Parameters.nb_case_board - 1) {
		for (j <- 0 to Parameters.nb_case_board - 1) {
		  Ksparov.curr_game.grids (k) (i + j * 8).enabled = false
		}
	  }
	}

    var tab = AlphaBeta.alphabeta(Ksparov.curr_game.board, Ksparov.curr_game.curr_player, Parameters.ai_speed)

    for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
	  for (i <- 0 to Parameters.nb_case_board - 1) {
		for (j <- 0 to Parameters.nb_case_board - 1) {
		  Ksparov.curr_game.grids (k) (i + j * 8).enabled = true
		}
	  }
	}

    def predicate( p : Piece) : Boolean = {
      return p.pos_x == tab(0)._1 && p.pos_y == tab(0)._2
    }

    if (tab(1) != (-1,-1)){
      var piece = Ksparov.curr_game.board.filter(predicate)(0)

    piece.move(tab(1)._1,tab(1)._2,Ksparov.curr_game.board)
    Ksparov.curr_game.last_move_dep = tab(0)._1 + tab(0)._2 * 8
    Ksparov.curr_game.last_move_arr = tab(1)._1 + tab(1)._2 * 8

    DrawActions.draw_game_board(Ksparov.curr_game.board)
    Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true

    if (Ksparov.curr_game.game_type > 7) {
      Ksparov.curr_game.write_to_the_pipe += (tab(0)._1 + 97).toChar + (tab(0)._2 + 1).toString + (tab(1)._1 + 97).toChar + (tab(1)._2 + 1).toString + "\n"
      Ksparov.curr_game.something_to_send = true
    }
    }
    else {
      Ksparov.curr_game.game_won = true
      DrawActions.draw_game_board(Ksparov.curr_game.board)
      Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true
    }
  }

  override def check_pat : Boolean = {
    pat
  }

  /** The promotion method for an ai player */
  def ai_promotion {
    Ksparov.curr_game.selected_promotion = "Queen"
    Ksparov.promotion (AlphaBeta.playerprom)
  }
}
