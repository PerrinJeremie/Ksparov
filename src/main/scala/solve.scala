class AI(player : Int) extends Player(player){
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
    if (Switches.curr_player == 1 ){
      Ksparov.joueur1.getmove
    }
    else{
      Ksparov.joueur0.getmove
    }
  }
}
