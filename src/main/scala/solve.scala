class AI(player : Int) extends Player(player){
	ai = true
  override def getmove {
    var already_check = new Array[Boolean](16)
    for(i <- 0 to 15) {
      if (Ksparov.board(i).pos_x < 0) {
        already_check (i) = true
      } else {
        already_check (i) = false
      }
    }
    val r = scala.util.Random
    var b = true
    while (b) {
      var ind = r.nextInt (16)
      if (!already_check (ind)) {
        already_check (ind) = true

        var t = Ksparov.board ((1 - id) * 16 + ind).possible_moves (Ksparov.board)
        if (t.nonEmpty) {
          b = false
          var (i,j) = t(r.nextInt(t.size))
          Ksparov.board((1 - id) * 16 + ind).move(i,j,Ksparov.board)
        }

        if (!((already_check.find (p => p == false)).nonEmpty) && b) {
          b = false
          Constants.game_nulle = true
        }        
      }
    }
    DrawActions.draw_game_board(Ksparov.board)
    Constants.players(Constants.curr_player).moved = true
  }
}
