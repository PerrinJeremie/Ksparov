import java.io._
import scala.io.Source
import sys.process._

/* On se base sur une version de PGN non ambigue donc chaque mouvement est de la forme (hormis les mouvements spéciaux tels que le roque et la promotion) :
 Na1(x)a2
 */


object Save{


  /* Liste de octuplet (irock,prom,piece_prom,piece,attack,check,p1,p2) avec
   - irock=0 si pas de roque, = 1 si grand, = -1 si petit. 
   - prom = true si il y a promotion, alors piece_prom indique la piece.
   - piece la piece qui bouge (K king ,Q queen ,B bishop ,N knight ,R rook).
   - attack = vrai si une piece est mangée
   - check s'il y a echec
   - p1 la position initiale, p2 la position d'arrivée */
  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))
  var list_of_moves : List[Moves] = List()
  var curr_move : Moves = (0,false,"","",false,false,(0,0),(0,0))
  var whowins = 2

  var add2_happened = false

  def is_valid (s:String) : Int = {
    var res : String = ("ls " + Constants.save_path) !!;
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

  def init : Unit = {
    list_of_moves = List()
  }

  def init_curr_move : Unit = {
    add2_happened = false
    curr_move = (0,false,"","",false,false,(0,0),(0,0))
  }

  def add_move1 (pos_piece : Int, p : (Int,Int)) : Unit = {
    val piece : Piece = Ksparov.board(pos_piece)
    val pre_information : (Boolean, Option[Piece], List[Piece]) = piece.pre_move(p._1,p._2, Ksparov.board)
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

  def add_move2 : Unit = {
    add2_happened = true
    list_of_moves = curr_move :: list_of_moves
  }

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

  def write_tags(writer : PrintWriter, event : String, site : String, date : String, round : String, white : String, black : String, result : String): Unit ={
    writer.write( "[ Event \"" + event + "\"]\n")
    writer.write( "[ Site \"" + site + "\"]\n")
    writer.write( "[ Date \"" + date + "\"]\n")
    writer.write( "[ Round \"" + round + "\"]\n")
    writer.write( "[ White \"" + white + "\"]\n")
    writer.write( "[ Black \"" + black + "\"]\n")
    writer.write( "[ Result \"" + result + "\"]\n")
    if(Constants.nb_grid == 2){
    writer.write( "[ Type \"Alice\" ]\n\n")}
  }

  def pos_to_PGN (p : (Int,Int)) : String = {
    (97 + p._1).toChar + (p._2 + 1).toString
  }

  def write_moves(writer : PrintWriter) : Unit = {

    def write_move(i : Int, move: Moves) : Unit = {
      if (i % 2 == 1){
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

  def write_to_file (s:String,event:String,site:String,date:String,round:String,white:String,black:String) : Int = {
    var result = whowins match { case 2 => "*" case 1 => "1-0" case 0 => "0-1" case -1 => "1/2-1/2"}
    is_valid(s+".pgn") match {
      case 0 =>
        val writer = new PrintWriter (new File (Constants.save_path + s + ".pgn" ))
        write_tags(writer,event,site,date,round,white,black,result)
        write_moves(writer)
        writer.write(" "+result) 
        writer.close()
        return 0
      case -1 =>
        return -1
      case -2 =>
        return -2
    }
  }

}

object Load {

  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))

  /* regexs to parse words and lines */

  val matchtag = """\[(.*)\"(.*)\"(.*)\]""".r
  val emptyline = """(' '|'\n'|'\t')*""".r
  val matchtag1 = """\[[^"_]*\"""".r
  val matchtag2 = """\"(.*)\"""".r
  val tourtag = """[0-9]+[.]+""".r
  val resulttag = """[*]|1-0|1–0|0-1|0–1|1/2-1/2|1/2–1/2""".r
  val droquereg = """0|O|O""".r
  val roquereg = """0–0|O–O|0-0|O-O|O-O""".r
  val piecetag = """(K|Q|B|N|R)+""".r
  val xaxistag = """[a-h]+""".r
  val yaxistag = """[1-8]+""".r
  val mattag = """[+ #]+""".r
  val spmessage = """(!!|["!?"]|["??"]|["?!"]|!$|["?$"])""".r


  var list_of_moves : List[String] = List()

  var infos :  Map[String,String] = Map()

  var player : Int = 1

  var finalresult : String = ""

  var specialmessage : String = ""

  var piece = ""
  var piece_Ch : Piece = new Pawn(0,-1,-1,0)
  var pos_init : (Int,Int) = (0,0)
  var pos_fin : (Int,Int) = (0,0)

  case class Exn_parsing_tags(message:String) extends Exception (message)

  def reset_when_parsing : Unit ={
    piece = ""
    piece_Ch  = new Pawn(0,-1,-1,0)
    pos_init = (0,0)
    pos_fin  = (0,0)
  }


  def pnotspace( c : Char) : Boolean = {
    return ( c != ' ' && c != '\t' && c != '\n')
  }

  def pnotspec( c : Char) : Boolean = {
    return ( c != '\"' && c != '[' )
  }


  class Reproducer(n : Int) extends Player(n : Int) {

    ai = true

    def is_x_axis (c : Char) : Boolean = {
      return xaxistag.findFirstIn(c.toString) match {
        case Some(s) => true
        case None => false
      }
    }

    def is_y_axis(c : Char) : Boolean = {
      yaxistag.findFirstIn(c.toString) match{
        case Some(s) => true
        case None => false
      }
    }

    def pcolumn( P : Piece) : Boolean = {
      return ((P.name == piece) && (P.player == Constants.curr_player) && P.pos_x == pos_init._1)
    }

    def pline( P : Piece) : Boolean = {
      return (P.name == piece && P.player == Constants.curr_player && P.pos_y == pos_init._2)
    }

    def pexactpos( P : Piece) : Boolean = {
      return (P.name == piece && P.player == Constants.curr_player && P.coords == pos_init)
    }

    def pcangoto( P :Piece): Boolean = {
      return (P.name == piece && (P.player == Constants.curr_player) && P.pre_move(pos_fin._1,pos_fin._2,Ksparov.board)._1)
    }

    def parse_word (s : String) : Unit = {

      s match {
        case resulttag(_*) =>
          finalresult = s
          Constants.game_won = true
          return ()
        case _ => ()
      }

      var w = s+ "$$$$$"

      /* Recherche du type de piéce */
      w(0).toString match{
        case droquereg(_*) =>
          w.substring(0,3) match{
            case roquereg(_*) =>
              Ksparov.board((1-Constants.curr_player)*16 +14).move(6,(1-Constants.curr_player)*7, Ksparov.board)
            case _ =>
              Ksparov.board((1-Constants.curr_player)*16 + 14).move(2,(1-Constants.curr_player)*7,Ksparov.board)
          }
          return ()
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
        piece_Ch = Ksparov.board.filter(pcolumn)(0)
        w = w.substring(1, w.length)
      }
      else{
        if( is_y_axis(w(0)) && (w(1) == 'x' || is_x_axis(w(1)))){
          pos_init = (0,w(0).toInt - 49)
          piece_Ch = Ksparov.board.filter(pline)(0)
          w = w.substring(1,w.length)
        }
        else{
          if (is_x_axis(w(0)) && is_y_axis(w(1)) && (w(2) == 'x' || is_x_axis(w(2)))){
            pos_init = ( w(0).toInt - 97, w(1).toInt - 49)
            piece_Ch =  Ksparov.board.filter(pexactpos)(0)
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
        piece_Ch = Ksparov.board.filter(pcangoto)(0)
      }

      w = w.substring(2,w.length)

      /* Promotion */
      if (w(0) == '='){
        w(1) match{
          case 'Q' => Constants.selected_promotion = "Queen"
          case 'N' => Constants.selected_promotion = "Knight"
          case 'B' => Constants.selected_promotion = "Bishop"
          case 'R' => Constants.selected_promotion = "Rook"
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

      Save.add_move1(Ksparov.board.indexOf(piece_Ch),pos_fin)
      piece_Ch.move(pos_fin._1,pos_fin._2,Ksparov.board)
      Save.add_move2

    }


    override def getmove : Unit = {
      if (!list_of_moves.isEmpty){
        reset_when_parsing
        try {
          parse_word(list_of_moves.head)
        }catch{
          case _ : Throwable => Constants.message_drawer = new DrawBoard.MessageDrawer ("<html><div style='text-align : center;'>Fichier Corrompu</html>")
        }
        list_of_moves = list_of_moves.tail
        DrawActions.draw_game_board(Ksparov.board)
        Constants.players(Constants.curr_player).moved = true
      }
    }

    override def check_pat : Boolean = {
      var sum = 0
      for (i <- 0 to Ksparov.board.length / 2 - 1) {
        sum = sum + Ksparov.board(i + 16 * (1 - id)).possible_moves(Ksparov.board).length
      }
      if (sum == 0) {
        Constants.game_nulle = true
        true
      } else {
        false
      }
    }

    override def ai_promotion : Unit = {
      Ksparov.promotion(Constants.curr_player)
    }
  }



  def get_list_move_from_file (filename : String)  : Unit = {

    infos = Map()
    infos += ("filename" -> filename)

    var i = 1

    for (lines <- Source.fromFile(Constants.save_path + filename + ".pgn").getLines()){
      lines match {
        case matchtag(_*) =>
          matchtag1.findFirstIn(lines) match{
            case Some(s1) =>
              matchtag2.findFirstIn(lines) match{
                case Some(s2) =>
                  var s11 = s1.filter(pnotspace).filter(pnotspec)
                  var s21 = s2.filter(pnotspec)
                  infos += ( s11 -> s21)
                  if (s11 == "Type" && s21 == "Alice") {
                    Constants.alice_chess = true
                  } else {
                    Constants.alice_chess = false
                  }
                case None => () /* Not possible */
              }
            case None => () /* Not possible */
          }
        case emptyline(_*) => ()
        case _ =>
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
    }
    list_of_moves = list_of_moves.reverse
  }


}
