class AI(player : Int) extends Player(player){
	ai = true
  var already_check = new Array[Boolean](16)
  override def getmove {
    /* This array is used not to try several times the same piece. 
       If the piece is dead, it should not be tried. */
    for(i <- 0 to 15) {
      if (Ksparov.board((1 - id) * 16 + i).pos_x < 0) {
        already_check (i) = true
      } else {
        already_check (i) = false
      }
    }

    val r = scala.util.Random
    var b = true

    /* While no movement has been done and there are still pieces to try, 
       search for a piece with a possible movement. */
    while (b) {
      var ind = r.nextInt (16)
      /* If the piece has not been tried, let's do it */
      if (!already_check (ind)) {
        already_check (ind) = true
        var t = Ksparov.board ((1 - id) * 16 + ind).possible_moves (Ksparov.board)
        /* If there are possible moves, select one of them and apply it */
        if (t.nonEmpty) {
          b = false
          var (i,j) = t(r.nextInt(t.size))
          Ksparov.board((1 - id) * 16 + ind).move(i,j,Ksparov.board)
        }
        /* If no move was played and there is no more pieces, the IA cannot move, thus the game is nulle */
        if (!((already_check.find (p => p == false)).nonEmpty) && b) {
          b = false
          Constants.game_nulle = true
        }        
      }
    }
    /* Draw the new board */
    DrawActions.draw_game_board(Ksparov.board)
    Constants.players(Constants.curr_player).moved = true
  }
}
