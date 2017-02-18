class AI(player : Int) extends Player(player){
	ia = true
  override def getmove(){
    val r = scala.util.Random
    var b = true
    while (b) {
      var ind = (1-id)*16 + r.nextInt(16)
      var t = Ksparov.board(ind).possible_moves(Ksparov.board)
      if (t.nonEmpty){
        b = false
        var (i,j) = t(0)
        Ksparov.board(ind).move(i,j,Ksparov.board)
      }
    }
    DrawActions.draw_game_board(Ksparov.board)
    Constants.game_type match {
      case 3 => 
        Switches.curr_player = 1 - id
      case _ => Ksparov.joueur1.getmove
    }
  }
}
