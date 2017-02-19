class AI(player : Int) extends Player(player){
	ai = true
  override def getmove {
    val r = scala.util.Random
    var b = true
    var i = 0
    while (b || i < 1000) {
      var ind = (1-id)*16 + r.nextInt(16)
      var t = Ksparov.board(ind).possible_moves(Ksparov.board)
      if (t.nonEmpty) {
        b = false
        var (i,j) = t(0)
        Ksparov.board(ind).move(i,j,Ksparov.board)
      }
      i += 1
    }
    DrawActions.draw_game_board(Ksparov.board)
    Constants.players(Constants.curr_player).moved = true
  }
}
