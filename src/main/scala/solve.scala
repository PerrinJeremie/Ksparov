class AI(player : Int) extends Player(player){
  override def getmove(){
    val r = scala.util.Random
    var b = true
    println(0)
    while (b) {
      var ind = (1-id)*16 + r.nextInt(16)
      var t = Ksparov.board(ind).possible_moves(Ksparov.board)
      if (t.nonEmpty){
        b = false
        var (i,j) = t(0)
        Ksparov.board(ind).move(i,j,Ksparov.board)
      }
    }
    
    if (Constants.game_type == 3){
      Thread.sleep(1000)
    }

    if (id == 1 ){
      Ksparov.joueur0.getmove
    }
    else{
      Ksparov.joueur1.getmove
    }
  }
}
