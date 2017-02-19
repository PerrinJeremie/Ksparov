class AI(player : Int) extends Player(player){
	ai = true
  override def getmove {
    var already_check = new Array[Boolean](16)
    for(i <- 0 to 15) {
      already_check (i) = false
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
          var (i,j) = t(0)
          Ksparov.board((1 - id) * 16 + ind).move(i,j,Ksparov.board)
        }
      }
    }
    DrawActions.draw_game_board(Ksparov.board)
    Constants.players(Constants.curr_player).moved = true
  }
}
