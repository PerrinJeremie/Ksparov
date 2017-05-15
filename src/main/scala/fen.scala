/** FEN notation */
object FEN{
  /** Returns the number of case without any piece on */
  def consecutive_nulls (g:Array[Array[Piece]],x:Int,y:Int) = {
    val len = g(x).size
    var consec_nulls = 0
    var j = y
    while(j < len && g(x)(j) == null){
      consec_nulls +=1
      j += 1
    }
    consec_nulls
  }
  /** Return the string of the FEN notation based on the board given in argument
  * @param g The board that will be transformed
  * @return A string in the FEN notation of the board
  */
  def board_to_FEN(g: Array[Piece]) : String = {
    //Initializing a board array for pratical use
    var board = new Array [ Array[Piece] ] (8)
    for (i<-(0 to 7) ){
      board(i) = new Array[Piece](8)
    }
    for (p<-g){
      var (x,y)=p.coords
      if (Aux.on_board(x,y)){
        board(7-y)(x) = p
      }
    }

    var st = new String
    var nulls = 0
    //Adding the pieces to the string
    for ( i<-0 to 7){
      if(i!=0){
        st+="/"
      }
      var j = 0
      while (j<=7){
        nulls = consecutive_nulls(board,i,j)
        if (nulls!=0){
          st+=nulls.toString
          j+=nulls
        }
        if (j <=7){
        var p = board(i)(j).asInstanceOf[Piece]
        var piece_symbol= (p.name,p.player) match {
          case  ("pawn",0) => "p"
          case  ("knight",0) => "n"
          case  ("bishop",0) => "b"
          case  ("rook",0) => "r"
          case  ("queen",0) => "q"
          case  ("king",0) => "k"
          case  ("pawn",1) => "P"
          case  ("knight",1) => "N"
          case  ("bishop",1) => "B"
          case  ("rook",1) => "R"
          case  ("queen",1) => "Q"
          case  ("king",1) => "K"
         }
        st+= piece_symbol
        }
        j+=1
      }
    }
    //Adding the current player
    var player = Ksparov.curr_game.curr_player match {
       case 0 => " b"
       case 1 => " w"
    }
    st+=player+" "
    //Adding the castling possibilities
    var castling_possible = false
    var kings = Ksparov.curr_game.kings
    //var kings = Array( new King (0, 4, 7, 0), new King (0,4,0,0))
    var rooks = g.filter ( (p:Piece) => p.name=="rook" )
    if (!kings(1).has_moved){
      for (r<-rooks){
        if (r.player==1 && !r.asInstanceOf[Rook].has_moved && r.pos_x == 7){
          st+="K"
          castling_possible = true
        }
        if (r.player==1 && !r.asInstanceOf[Rook].has_moved  && r.pos_x == 0){
          st+="Q"
          castling_possible = true
        }
      }
    }
    if (!kings(0).has_moved){
      for (r<-rooks){
        if (r.player==0 && !r.asInstanceOf[Rook].has_moved  && r.pos_x == 7){
          st+="k"
          castling_possible = true
        }
        if (r.player==0 && !r.asInstanceOf[Rook].has_moved  && r.pos_x == 0){
          st+="q"
          castling_possible = true
        }
      }
    }
    if (!castling_possible){
      st+="-"
    }
    st+=" "
    //getting last played move
    if (Save.list_of_moves.nonEmpty){
    var (irock,prom,piece_prom,piece, attack, check, p1,p2) = Save.list_of_moves.head
    val deplacement = math.abs(p2._2-p1._2) // Has it moved two cases ?
    if (piece=="" && deplacement == 2){
      var (x,y)= (p2._1, p2._2)
      st+= (x+97).toChar + (y+1-math.signum(p2._2- p1._2)).toString +" "
    }
    else{
    st+="- "
    }
   }
else{
  st+="- "
}
var bor_moves = Ksparov.curr_game.nb_boring_moves
st+= bor_moves.toString
st+= " 1"
st
 }
}
