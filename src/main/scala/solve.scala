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
    /* All piece are initially possible, so the array is false, 
       but if a piece is dead (x negative), it should not be tried to move */
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

    /* While no movement has been done and there are still pieces to try, 
       search for a piece with a possible movement. */
    while (notdone) {
      /** The random piece seleted */
      var ind = r.nextInt (16)
      /* If the piece has not been tried, let's do it */
      if (!already_check (ind)) {
        already_check (ind) = true
        /** The list of possible moves for the random piece */
        var t = Ksparov.curr_game.board ((1 - id) * 16 + ind).possible_moves (Ksparov.curr_game.board)
        /* If there are possible moves, select one of them and apply it */
        if (t.nonEmpty) {
          notdone = false
          var (i,j) = t(r.nextInt(t.size))
          /* Save move */
          Save.add_move1((1 - id) * 16 + ind, (i,j))
          /* Play move */
          Ksparov.curr_game.board((1 - id) * 16 + ind).move(i,j,Ksparov.curr_game.board)
        }
        /* If no move was played and there is no more pieces left, the IA cannot move, thus the game is nulle */
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
    /* Draw the new board */
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
  override def run {
    // While threads should be alive
    while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
      // We wait a time defines in parameters
      Thread.sleep (Parameters.ai_speed)
      // If it is our turn, we move a piece
      if (Ksparov.curr_game.ai_turn) {
        Ksparov.play_move
        Ksparov.curr_game.ai_turn = false
      }
    }
  }
}