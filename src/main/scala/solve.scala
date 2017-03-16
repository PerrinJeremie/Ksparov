class AI (player : Int) extends Player (player) {

  var pat = false

	ai = true
  /* This array is used not to try several times the same piece. */
  var already_check = new Array[Boolean](16)

  override def getmove {

    /* All piece are initially possible, so the array is false, 
       but if a piece is dead (x negative), it should not be tried to move */
    for(i <- 0 to 15) {
      if (Ksparov.board((1 - id) * 16 + i).pos_x < 0) {
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
        var t = Ksparov.board ((1 - id) * 16 + ind).possible_moves (Ksparov.board)
        /* If there are possible moves, select one of them and apply it */
        if (t.nonEmpty) {
          notdone = false
          var (i,j) = t(r.nextInt(t.size))
          /* Save move */
          Save.add_move1((1-id)*16 + ind, (i,j))
          /* Play move */
          Ksparov.board((1 - id) * 16 + ind).move(i,j,Ksparov.board)
        }
        /* If no move was played and there is no more pieces left, the IA cannot move, thus the game is nulle */
        if (!((already_check.find (p => p == false)).nonEmpty) && notdone) {
          notdone = false
          Constants.players(Constants.curr_player).asInstanceOf[AI].pat = true
          Constants.game_nulle = true
          Constants.curr_player = 1 - Constants.curr_player
        }        
      }
    }
    Save.add_move2
    /* Draw the new board */
    DrawActions.draw_game_board(Ksparov.board)
    Constants.players(Constants.curr_player).moved = true
  }

  override def check_pat : Boolean = {
    pat
  }
}

object AI_methods {
  def ai_promotion {
    val rand = scala.util.Random
    rand.nextInt(4) match {
      case 0 => Constants.selected_promotion = "Queen"
      case 1 => Constants.selected_promotion = "Bishop"
      case 2 => Constants.selected_promotion = "Knight"
      case 3 => Constants.selected_promotion = "Rook"
    }
    Ksparov.promotion (Constants.curr_player)
  } 
}
