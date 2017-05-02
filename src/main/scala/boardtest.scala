

object BoardsT {
def init_alt_board (s : String) = {
  s match {
    case "prom" =>
      for (p <- 0 to 1) {
        for(i <- 0 to 7) {
          Ksparov.curr_game.board((1 - p) * 16 + i) = new Pawn(p, i, 1 + (1-p)* 5, 0)
        }
        Ksparov.curr_game.board(8 + (1 - p) * 16) = new Rook(p, -1, -1, 0)
        Ksparov.curr_game.board(9 + (1 - p) * 16) = new Rook(p, -1, -1, 0)
        Ksparov.curr_game.board(10 + (1 - p) * 16) = new Knight(p, -1, -1, 0)
        Ksparov.curr_game.board(11 + (1 - p) * 16) = new Knight(p, -1, -1, 0)
        Ksparov.curr_game.board(12 + (1 - p) * 16) = new Bishop(p, -1, -1, 0)
        Ksparov.curr_game.board(13 + (1 - p) * 16) = new Bishop(p, -1, -1, 0)
        // Kings are instantiate in a Game to have a quick access to them
        Ksparov.curr_game.kings(p) = new King (p, 4, 7, 0)
        Ksparov.curr_game.board(14 + (1 - p) * 16) = Ksparov.curr_game.kings(p)
        Ksparov.curr_game.board(15 + (1 - p) * 16) = new Queen(p, -1, -1, 0)
      }
    case "roque" => 
      for (p <- 0 to 1) {
        for(i <- 0 to 7) {
          Ksparov.curr_game.board((1 - p) * 16 + i) = new Pawn(p, -1, -1, 0)
        }
        Ksparov.curr_game.board(8 + (1 - p) * 16) = new Rook(p, (-1 * (1- p)) + 0,(-1 * p)* (1 - p) * 7, 0)
        Ksparov.curr_game.board(9 + (1 - p) * 16) = new Rook(p, -7, -(1 - p) * 7, 0)
        Ksparov.curr_game.board(10 + (1 - p) * 16) = new Knight(p, -1, -(1 - p) * 7, 0)
        Ksparov.curr_game.board(11 + (1 - p) * 16) = new Knight(p, -6, -(1 - p) * 7, 0)
        Ksparov.curr_game.board(12 + (1 - p) * 16) = new Bishop(p, -2, -(1 - p) * 7, 0)
        Ksparov.curr_game.board(13 + (1 - p) * 16) = new Bishop(p, -5, -(1 - p) * 7, 0)
        // Kings are instantiate in a Game to have a quick access to them 
        Ksparov.curr_game.board(15 + (1 - p) * 16) = new Queen(p, -3, -(1 - p) * 7, 0)
      }
      Ksparov.curr_game.kings(0) = new King(0, 3, 7, 0)
      Ksparov.curr_game.kings(1) = new King(1, 4, 0, 0)
      Ksparov.curr_game.board(14) = Ksparov.curr_game.kings(1)
      Ksparov.curr_game.board(30) = Ksparov.curr_game.kings(0)
    case "legal" => 
      for (p <- 0 to 1) {
        for(i <- 0 to 7) {
          Ksparov.curr_game.board((1 - p) * 16 + i) = new Pawn(p, i, 1 + (1 - p) * 5, 0)
        }
        Ksparov.curr_game.board(8 + (1 - p) * 16) = new Rook(p, 0, (1 - p) * 7, 0)
        Ksparov.curr_game.board(9 + (1 - p) * 16) = new Rook(p, 7, (1 - p) * 7, 0)
        Ksparov.curr_game.board(10 + (1 - p) * 16) = new Knight(p, 1, (1 - p) * 7, 0)
        Ksparov.curr_game.board(11 + (1 - p) * 16) = new Knight(p, 6, (1 - p) * 7, 0)
        Ksparov.curr_game.board(12 + (1 - p) * 16) = new Bishop(p, 2, (1 - p) * 7, 0)
        Ksparov.curr_game.board(13 + (1 - p) * 16) = new Bishop(p, 5, (1 - p) * 7, 0)
        // Kings are instantiate in a Game to have a quick access to them
        Ksparov.curr_game.board(14 + (1 - p) * 16) = Ksparov.curr_game.kings(p)
        Ksparov.curr_game.board(15 + (1 - p) * 16) = new Queen(p, 3, (1 - p) * 7, 0)
      }
  }
}
}
