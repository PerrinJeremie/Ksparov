class AI (player : Int) extends Player (player) {

  var pat = false

	ai = true
  /* This array is used not to try several times the same piece. */
  var already_check = new Array[Boolean](16)

  override def getmove {

    /* All piece are initially possible, so the array is false, 
       but if a piece is dead (x negative), it should not be tried to move */
    for(i <- 0 to 15) {
      if (Ksparov.curr_game.board((1 - id) * 16 + i).pos_x < 0) {
        already_check (i) = true
      } else {
        already_check (i) = false
      }
    }

    val r = scala.util.Random
    var notdone = true

    /* While no movement has been done and there are still pieces to try, 
       search for a piece with a possible movement. */
    while (notdone) {
      var ind = r.nextInt (16)
      /* If the piece has not been tried, let's do it */
      if (!already_check (ind)) {
        already_check (ind) = true
        var t = Ksparov.curr_game.board ((1 - id) * 16 + ind).possible_moves (Ksparov.curr_game.board)
        /* If there are possible moves, select one of them and apply it */
        if (t.nonEmpty) {
          notdone = false
          var (i,j) = t(r.nextInt(t.size))
          /* Save move */
          Save.add_move1((1-id)*16 + ind, (i,j))
          /* Play move */
          Ksparov.curr_game.board((1 - id) * 16 + ind).move(i,j,Ksparov.curr_game.board)
        }
        /* If no move was played and there is no more pieces left, the IA cannot move, thus the game is nulle */
        if (!((already_check.find (p => p == false)).nonEmpty) && notdone) {
          notdone = false
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).asInstanceOf[AI].pat = true
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

  override def check_pat : Boolean = {
    pat
  }

  def ai_promotion {

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

class AIMoveThread extends Thread {
  override def run {
    while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
      Thread.sleep (Parameters.ai_speed)
      if (Ksparov.curr_game.ai_turn) {
        Ksparov.play_move
        Ksparov.curr_game.ai_turn = false
      }
    }
  }
}