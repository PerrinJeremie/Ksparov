import java.io._
import scala.io.Source
import sys.process._
import scala.language.postfixOps


/** Object which regroups the functions and values associated with saving games.*/
object Save{


  /** tuple (irock,prom,piece_prom,piece,attack,check,p1,p2) with
   - irock=0 if no roque, = 1 queen-side, = -1 if king-side. 
   - prom = true if promotion occurs, then piece_prom indicates chosen piece.
   - piece : the piece qui which moves (K king ,Q queen ,B bishop ,N knight ,R rook).
   - attack = true if a piece is eaten
   - check = true if check occurs
   - p1 initial position, p2 final position of piece */
  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))

  /** list of moves being filled throughout game */
  var list_of_moves : List[Moves] = List()

  /** current move filled through the getmove method of a Player and the promotion method of object Ksparov. */
  var curr_move : Moves = (0,false,"","",false,false,(0,0),(0,0))

  /** stores the won status of game : -1 if null, 0 if black wins, 1 if white, 2 if nothing has been decided. */
  var whowins = 2

  /** a switch to control wether promotion has to rewrite move or to change curr_move.*/
  var add2_happened = false

  /** tests the validity of the string argument as a filename for the save : -2 if too long, -1 if already exists. */
  def is_valid (s:String) : Int = {
    var res : String = ("ls " + Display.save_path) !!;
    if(s.length > 34){
      return -2
    }
    else{
      if(res.indexOf(s) == -1){
        return 0
      }
      else{
        -1
      }
    }
  }

  /** resets the list_of_moves value. */
  def init : Unit = {
    list_of_moves = List()
  }

  /** resets the curr_move value. */
  def init_curr_move : Unit = {
    add2_happened = false
    curr_move = (0,false,"","",false,false,(0,0),(0,0))
  }

  /** To be called in the getmove method of a Player right before the applied move, loads part of curr_move. */  
  def add_move1 (pos_piece : Int, p : (Int,Int)) : Unit = {
    val piece : Piece = Ksparov.curr_game.board(pos_piece)
    val pre_information : (Boolean, Option[Piece], List[Piece]) = piece.pre_move(p._1,p._2, Ksparov.curr_game.board)
    val s = piece.name match {
      case "pawn" => ""
      case "rook" => "R"
      case "bishop" => "B"
      case "queen" => "Q"
      case "knight" => "N"
      case "king" => "K"
    }
    init_curr_move
    if (pre_information._1){

      curr_move = ( curr_move._1, curr_move._2, curr_move._3, s , curr_move._5,!pre_information._3.isEmpty,piece.coords,p)

      pre_information._2 match{
        case None => ()
        case Some(piece_mange) =>
          if (piece_mange.player == piece.player){
            if (piece_mange.pos_x == 7){
              curr_move = ( -1 , curr_move._2, curr_move._3, curr_move._4, curr_move._5,curr_move._6,curr_move._7,curr_move._8 )
            }
            else{
              curr_move = ( 1, curr_move._2, curr_move._3, curr_move._4, curr_move._5,curr_move._6,curr_move._7,curr_move._8 )
            }
          }
          else{
            curr_move = ( curr_move._1, curr_move._2, curr_move._3, curr_move._4, true,curr_move._6,curr_move._7,curr_move._8 )
          }
      }
    }
  }

  /** To be called in the getmove method of a Player after the applied move if it succeeded, appends curr_move to list_of_moves. */
  def add_move2 : Unit = {
    add2_happened = true
    list_of_moves = curr_move :: list_of_moves
  }

  /** Called in the promotion method of object Ksparov, modifies curr_move. If add2_happened = true, replaces the head of list_of_moves by the new value of curr_move. */
  def add_prom_to_move( s: String, b:Boolean) : Unit ={
    val piece_prom : String =
      s match {
        case "Knight" => "N"
        case "Bishop" => "B"
        case "Rook" => "R"
        case "Queen" => "Q"
      }
    curr_move = (curr_move._1,true,piece_prom,curr_move._4,curr_move._5,curr_move._6 || b,curr_move._7,curr_move._8)
    if ( add2_happened) {
      list_of_moves = curr_move :: list_of_moves.tail
    }
  }

  /*  def init : String = {
   val r = scala.util.Random
   var name  = "0000000000000000"
   while ! (is_valid(name + ".pgn")) {
   name = ""
   for( i <- 0 to 15){
   name = name + (r.nextInt(10)).toString
   }
   }
   return name + ".pgn"
   }*/

  /** Writes the tag of the .PGN file.*/
  def write_tags(writer : PrintWriter, event : String, site : String, date : String, round : String, white : String, black : String, result : String): Unit = {
    writer.write ("[ Event \"" + event + "\"]\n")
    writer.write ("[ Site \"" + site + "\"]\n")
    writer.write ("[ Date \"" + date + "\"]\n")
    writer.write ("[ Round \"" + round + "\"]\n")
    writer.write ("[ White \"" + white + "\"]\n")
    writer.write ("[ Black \"" + black + "\"]\n")
    writer.write ("[ Result \"" + result + "\"]\n")
    if (Ksparov.curr_game.alice_chess) {
      writer.write ("[ Type \"Alice"+ Parameters.nb_alice_board.toString + "\" ]\n\n")
    }
  }

  /** Translates (i,j) into (aj : String) where aj is the board representation of the position (x,y).*/
  def pos_to_PGN (p : (Int,Int)) : String = {
    (97 + p._1).toChar + (p._2 + 1).toString
  }

  /** Write all moves in the previously opened file */
  def write_moves(writer : PrintWriter) : Unit = {

    def write_move(i : Int, move: Moves) : Unit = {
      if (i % 2 == 1){
        if (Ksparov.curr_game.alice_chess){
          writer.write("%")}
        writer.write( ((i+1)/2).toString + ".")
      }
      move._1 match{
        case -1 => writer.write( " 0-0 " )
        case 1 => writer.write( " 0-0-0 " )
        case 0 =>
          writer.write(" ")
          writer.write( move._4 + pos_to_PGN(move._7))
          if(move._5){
            writer.write("x")
          }
          writer.write(pos_to_PGN(move._8))
          if (move._2){
            writer.write ("=" + move._3)
          }
          if(move._6){
            writer.write("+ ")
          }
      }
      if (i % 2 == 0){
        writer.write( "\n")
      }
    }

    def rec_writing(l_of_moves : List[Moves]) : Int = {
      if (  ! l_of_moves.isEmpty ){
        val i = rec_writing(l_of_moves.tail)
        write_move(i,l_of_moves.head)
        return (i+1)
      }
      else{
        return 1
      }
    }
    val _ = rec_writing(list_of_moves)
  }

  /* returns 0 if all went well, -1 if it's not the case.
   Informations have to be filled by players except for result which is the result of the game :
   with "1/2-1/2" if null, "1-0" if white won, "0-1" if black won, "*" if unfinished */
  /** Returns 0 if all went well. Opens a file, writes tags, then moves, then result into said file.*/
  def write_to_file (s : String, event : String, site : String, date : String, round : String, white : String, black : String) : Int = {
    var result = whowins match { case 2 => "*" case 1 => "1-0" case 0 => "0-1" case -1 => "1/2-1/2"}
    is_valid(s+".pgn") match {
      case 0 =>
        date match{
          case Load.datereg(_*) =>
            val writer = new PrintWriter (new File (Display.save_path + s + ".pgn" ))
            write_tags(writer, event, site, date, round, white, black, result)
            write_moves(writer)
            writer.write(" "+result)
            writer.close()
            return 0
          case _ =>
            return -3
        }
      case -1 =>
        return -1
      case -2 =>
        return -2
    }
  }

}

/** Object which regroups functions and values associated with loading a game.*/
object Load {

  /** Refer to Save.Moves */
  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))

  /* regexs to parse words and lines */
  /**Recognizes tags*/
  val matchtag = """\[(.*)\"(.*)\"(.*)\]""".r
  /**Recognizes empty lines*/
  val emptyline = """(' '|'\n'|'\t')*""".r
  /**Recognizes first part of tag*/
  val matchtag1 = """\[[^"_]*\"""".r
  /**Recognizes second part of tag*/
  val matchtag2 = """\"(.*)\"""".r
  /**Recognizes a turn of PGN*/
  val tourtag = """[0-9]+[.]+""".r
  /**Recognizes a result of PGN*/
  val resulttag = """[*]|1-0|1–0|0-1|0–1|1/2-1/2|1/2–1/2""".r
  /**Recognizes the beginning of a roque*/
  val droquereg = """0|O|O""".r
  /**Recognizes a king-side roque of PGN*/
  val roquereg = """0–0|O–O|0-0|O-O|O-O""".r
  /**Recognizes a piece character of PGN*/
  val piecetag = """(K|Q|B|N|R)+""".r
  /**Recognizes a value of position on x axis of PGN*/
  val xaxistag = """[a-h]+""".r
  /**Recognizes a value of position on y axis of PGN */
  val yaxistag = """[1-8]+""".r
  /**Recognizes a mat character of PGN*/
  val mattag = """[+ #]+""".r
  /**Recognizes a special annotation after a move*/
  val spmessage = """(!!|["!?"]|["??"]|["?!"]|!$|["?$"])""".r
  /**Recognizes a date format YYYY.MM.DD*/
  val datereg = """\d\d\d\d[.](0[1-9]|1[0 1 2])[.]([0 1 2]\d|3[0 1])""".r
  /**Recognizes a middle of line comment of PGN*/
  val comments = "(.*);(.*)".r 
  /**Recognizes the extension annotation of PGN*/
  val extension = "%(.*)".r

  /** To store the PGN format of moves.*/
  var list_of_moves : List[String] = List()

  /** A map which encapsulates the tags.*/
  var infos :  Map[String,String] = Map()

  /* var player : Int = 1 */
  /** Stores the final result when it is read in the list_of_moves value.*/
  var finalresult : String = ""

  /** Locally stores the read special message in the getmove method of a Reproducer. */
  var specialmessage : String = ""

  /** Locally stores the name of the piece moved in the getmove method of a Reproducer. */
  var piece = ""
  var piece_Ch : Piece = new Pawn(0,-1,-1,0)
  var pos_init : (Int,Int) = (0,0)
  var pos_fin : (Int,Int) = (0,0)

  /*case class Exn_parsing_tags(message:String) extends Exception (message)*/

  /** Resets the local variables at the beginning of the getmove method of a Reproducer */
  def reset_when_parsing : Unit ={
    piece = ""
    piece_Ch  = new Pawn(0,-1,-1,0)
    pos_init = (0,0)
    pos_fin  = (0,0)
  }

  /** Predicates true if char is not a blank space character */
  def pnotspace (c : Char) : Boolean = {
    return ( c != ' ' && c != '\t' && c != '\n')
  }

  /** Predicates true if char is not " nor [ */
  def pnotspec( c : Char) : Boolean = {
    return ( c != '\"' && c != '[' )
  }

  /** Represents a robot player which can read from Load.list_of_moves a move and play it, it is he who understands PGN move notation */
  class Reproducer(n : Int) extends Player(n : Int) {

    /** It is a robot */
    ai = true

    /** Predicates if char is [a-h] */
    def is_x_axis (c : Char) : Boolean = {
      return xaxistag.findFirstIn(c.toString) match {
        case Some(s) => true
        case None => false
      }
    }

    /** Predicates if char is [1-8] */
    def is_y_axis(c : Char) : Boolean = {
      yaxistag.findFirstIn(c.toString) match{
        case Some(s) => true
        case None => false
      }
    }

    /** Predicates if piece is in column of pos_init */
    def pcolumn( P : Piece) : Boolean = {
      return ((P.name == piece) && (P.player == Ksparov.curr_game.curr_player) && P.pos_x == pos_init._1)
    }

    /** Predicates if piece is in pos_init's line*/
    def pline( P : Piece) : Boolean = {
      return (P.name == piece && P.player == Ksparov.curr_game.curr_player && P.pos_y == pos_init._2)
    }

    /** Predicates if piece is in pos_init position */ 
    def pexactpos( P : Piece) : Boolean = {
      return (P.name == piece && P.player == Ksparov.curr_game.curr_player && P.coords == pos_init)
    }

    /** Predicates if piece can go to pos_fin position */ 
    def pcangoto( P :Piece): Boolean = {
      return (P.name == piece && (P.player == Ksparov.curr_game.curr_player) && P.pre_move(pos_fin._1,pos_fin._2,Ksparov.curr_game.board)._1)
    }

    /** Finds all information related to the move and plays the move once done */
    def parse_word (s : String) : Boolean = {

      s match {
        case resulttag(_*) =>
          finalresult = s
          Ksparov.check_game_status(Ksparov.curr_game.curr_player)
          if (! (s == "*") ) {Ksparov.curr_game.game_won = true; return true} else { return false }
        case _ => ()
      }

      var w = s+ "$$$$$"

      /* Recherche du type de piéce */
      w(0).toString match{
        case droquereg(_*) =>
          w.substring(0,3) match{
            case roquereg(_*) =>
              Ksparov.curr_game.board((1-Ksparov.curr_game.curr_player)*16 +14).move(6,(1-Ksparov.curr_game.curr_player)*7, Ksparov.curr_game.board)
            case _ =>
              Ksparov.curr_game.board((1-Ksparov.curr_game.curr_player)*16 + 14).move(2,(1-Ksparov.curr_game.curr_player)*7,Ksparov.curr_game.board)
          }
          return true
        case "K" => piece = "king"; w = w.substring(1, w.length)
        case "Q" => piece = "queen"; w = w.substring(1, w.length)
        case "B" => piece = "bishop"; w = w.substring(1, w.length)
        case "N" => piece = "knight"; w = w.substring(1, w.length)
        case "R" => piece = "rook"; w = w.substring(1, w.length)
        case "P" => piece = "pawn"; w = w.substring(1, w.length)
        case _ => piece = "pawn"
      }

      /*Desambiguisont le mouvement */

      if ( is_x_axis(w(0)) && (w(1) == 'x' || is_x_axis(w(1)))){
        pos_init = (w(0).toInt - 97,0)
        piece_Ch = Ksparov.curr_game.board.filter(pcolumn)(0)
        w = w.substring(1, w.length)
      }
      else{
        if( is_y_axis(w(0)) && (w(1) == 'x' || is_x_axis(w(1)))){
          pos_init = (0,w(0).toInt - 49)
          piece_Ch = Ksparov.curr_game.board.filter(pline)(0)
          w = w.substring(1,w.length)
        }
        else{
          if (is_x_axis(w(0)) && is_y_axis(w(1)) && (w(2) == 'x' || is_x_axis(w(2)))){
            pos_init = ( w(0).toInt - 97, w(1).toInt - 49)
            piece_Ch =  Ksparov.curr_game.board.filter(pexactpos)(0)
            w = w.substring(2,w.length)
          }
        }
      }
      /* Mange une piéce */
      if (w(0) == 'x'){
        w = w.substring(1,w.length)
      }

      /* Déplacement */

      pos_fin = (w(0).toInt - 97, w(1).toInt - 49)

      if(piece_Ch.coords == (-1,-1)){
        piece_Ch = Ksparov.curr_game.board.filter(pcangoto)(0)
      }

      w = w.substring(2,w.length)

      /* Promotion */
      if (w(0) == '='){
        w(1) match{
          case 'Q' => Ksparov.curr_game.selected_promotion = "Queen"
          case 'N' => Ksparov.curr_game.selected_promotion = "Knight"
          case 'B' => Ksparov.curr_game.selected_promotion = "Bishop"
          case 'R' => Ksparov.curr_game.selected_promotion = "Rook"
        }
        w = w.substring(2,w.length)
      }

      /* Mat */
      w(0).toString match{
        case mattag(_*) => w = w.substring(1,w.length)
        case _ => ()
      }

      /* Comments */
      w.substring(0,2) match{
        case spmessage(s) => specialmessage = s
        case _ => ()
      }

      Save.add_move1(Ksparov.curr_game.board.indexOf(piece_Ch),pos_fin)
      piece_Ch.move(pos_fin._1,pos_fin._2,Ksparov.curr_game.board)
      Save.add_move2
      return true

    }

    /** the getmove method applies parse_word and prints board*/
    override def getmove : Unit = {
      if (!list_of_moves.isEmpty){
        reset_when_parsing
        try {
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = parse_word(list_of_moves.head)
        }catch{
          case _ : Throwable => Ksparov.curr_game.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Fichier Corrompu</html>")
        }
        list_of_moves = list_of_moves.tail
        DrawActions.draw_game_board(Ksparov.curr_game.board)
      }
    }

    /** Same check_pat method as AI's*/
    override def check_pat : Boolean = {
      var sum = 0
      for (i <- 0 to Ksparov.curr_game.board.length / 2 - 1) {
        sum = sum + Ksparov.curr_game.board(i + 16 * (1 - id)).possible_moves(Ksparov.curr_game.board).length
      }
      if (sum == 0) {
        Ksparov.curr_game.game_nulle = true
        true
      } else {
        false
      }
    }

    /** Calls the promotion method of Ksparov object */ 
    override def ai_promotion : Unit = {
      Ksparov.promotion(Ksparov.curr_game.curr_player)
    }
  }

  /** reads a move and adds it to list_of_moves */
  def read_moves (lines : String) : Unit = {
    if (!infos.contains("White")) {
          infos += ("White" -> "Garry Kasparov") }
        if (!infos.contains("Black")) {
          infos += ("Black" -> "Bobby Fischer") }
        val array_of_words = lines.split(' ')
        for (i <- 0 to array_of_words.length -1) {
          array_of_words(i) match {
            case tourtag(_*) => ()
            case "" => ()
            case _ => val arr = array_of_words(i).split('.')
              list_of_moves = arr(arr.length -1) :: list_of_moves
          }
        }
  }

  /** reads a line and treats it differently depending of PGN requirements */
  def read_line (lines : String) : Unit = {
    lines match {
      case matchtag(_*) =>
        matchtag1.findFirstIn(lines) match {
          case Some(s1) =>
            matchtag2.findFirstIn(lines) match {
              case Some(s2) =>
                var s11 = s1.filter(pnotspace).filter(pnotspec)
                var s21 = s2.filter(pnotspec)
                infos += ( s11 -> s21)
                if (s11 == "Type") {
                  var alicereg = """.*[A a]lice(\d*)""".r
                  s21 match{
                    case alicereg(n) =>
                      Ksparov.curr_game.alice_chess = true
                      Ksparov.curr_game.nb_grid = n.toInt
                    case _ => ()
                  }
                } else {
                  Ksparov.curr_game.alice_chess = false
                }
              case None => () /* Not possible */
            }
          case None => () /* Not possible */
        }
      case emptyline(_*) => ()
      case extension(s) => if(Ksparov.curr_game.alice_chess){
                              read_moves(s)
                           } else {
                              ()
                           }
      case comments(s1,s2) => read_moves(s1)
      case _ => read_moves(lines)
    }
  }

  /** Reads from file. filename is the name of the file without the .pgn extension. It escapes variations and comments and linefeed character.*/
  def get_list_move_from_file (filename : String)  : Unit = {

    infos = Map()
    infos += ("filename" -> filename)

    var i = 1
    var texte_entier = ""

    for (lines <- Source.fromFile(Display.save_path + filename + ".pgn").getLines()) {
      texte_entier += lines + "\n"
    }

    var tableau = texte_entier.split("\\(|\\)").zipWithIndex.filter(x => x._2 % 2 == 0).map(_._1)
    texte_entier = tableau.mkString.split("\\{|\\}").zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    texte_entier.split('\n').iterator.foreach(read_line)
    list_of_moves = list_of_moves.reverse
  }


}
