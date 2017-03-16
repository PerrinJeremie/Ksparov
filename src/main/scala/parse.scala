import java.io._
import scala.io.Source
import sys.process._

/* On se base sur une version de PGN non ambigue donc chaque mouvement est de la forme (hormis les mouvements spéciaux tels que le roque et la promotion) :
 Na1(x)a2
 */


object Save{

  /* Liste de triplets (irock,prom,piece_prom,piece,p1,p2) avec
   - irock=0 si pas de roque, = 1 si grand, = -1 si petit. 
   - prom = true si il y a promotion, alors piece_prom indique la piece.
   - piece la piece qui bouge (K king ,Q queen ,B bishop ,N knight ,R rook).
   - attack = vrai si une piece est mangée
   - check s'il y a echec
   - p1 la position initiale, p2 la position d'arrivée */
  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))
  var list_of_moves : List[Moves] = List()
  var curr_move : Moves = (0,false,"","",false,false,(0,0),(0,0))

  var add2_happened = false

  def is_valid (s:String) : Boolean = {
    var res : String = ("ls " + Constants.save_path) !!;
    return (res.indexOf(s) == -1 && s.length <= 24)
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
    writer.write( "[ Result \"" + result + "\"]\n\n")
  }

  def pos_to_PGN (p : (Int,Int)) : String = {
    (97 + p._1).toChar + (p._2 + 1).toString
  }

  def write_moves(writer : PrintWriter, result : String) : Unit = {

    def write_move(i : Int, move: Moves) : Unit = {
      if (i % 2 == 1){
        writer.write( ((i+1)/2).toString + ".")
      }
      move._1 match{
        case -1 => writer.write( " O-O " )
        case 1 => writer.write( " O-O-O " )
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

  def write_to_file (s:String,event:String,site:String,date:String,round:String,white:String,black:String,result:String) : Int = {
    if (is_valid(s+".pgn")) {
      val writer = new PrintWriter (new File (Constants.save_path + s + ".pgn" ))
      write_tags(writer,event,site,date,round,white,black,result)
      write_moves(writer,result)
      writer.close()
      return 0
    }
    else {
      return -1
    }
  }

}

object Load {

  type Moves = (Int , Boolean , String , String , Boolean, Boolean, (Int,Int), (Int,Int))

  /* regexs to parse words and lines */

  val matchtag = """"(.*)"""".r
  val tourtag = """[0-9]+[.]""".r
  val piecetag = """(K|Q|B|N|R)+""".r
  val xaxistag = """[a-h]+""".r
  val yaxistag = """[1-8]+""".r

  var list_of_moves : List[String] = List()
  
  var infos :  Array[String] = Array("","","","","","","","")

  var player : Int = 1

  var piece = ""
  var piece_Ch : Piece = new Pawn(0,-1,-1,0)
  var pos_init : (Int,Int) = (0,0)
  var pos_fin : (Int,Int) = (0,0)

  def reset_when_parsing : Unit ={
    var piece = ""
    var piece_Ch : Piece = new Pawn(0,-1,-1,0)
    var pos_init : (Int,Int) = (0,0)
    var pos_fin : (Int,Int) = (0,0)
  }

  class Reproducer(n : Int) extends Player(n:Int){

    ai = true

    def is_x_axis(c : Char) : Boolean = {
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
      return ((P.name == piece) && (P.player == id) && P.pos_x == pos_init._1)
    }

    def pline( P : Piece) : Boolean = {
      return (P.name == piece && P.player == id && P.pos_y == pos_init._2)
    }

    def pexactpos( P : Piece) : Boolean = {
      return (P.name == piece && P.player == id && P.coords == pos_init)
    }

    def pcangoto( P :Piece): Boolean = {
      return (P.name == piece && P.player == id && P.pre_move(pos_fin._1,pos_fin._2,Ksparov.board)._1)
    }

    def parse_word (s : String) : Unit = {
      var w = s+ "$$$$$"

      /* Recherche du type de piéce */

      w(0) match{
        case 'O' =>
          if (w == "O-O"){
            Ksparov.board((1-id)*16 +14).move(6,(1-id)*7, Ksparov.board)
          }
          else{
            Ksparov.board((1-id)*16 + 14).move(2,(1-id)*7,Ksparov.board)
          }
          return ()
        case 'K' => piece = "king"; w = w.substring(1, w.length)
        case 'Q' => piece = "queen"; w = w.substring(1, w.length)
        case 'B' => piece = "bishop"; w = w.substring(1, w.length)
        case 'N' => piece = "knight"; w = w.substring(1, w.length)
        case 'R' => piece = "rook"; w = w.substring(1, w.length)
        case 'P' => piece = "pawn"; w = w.substring(1, w.length)
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
            println( pos_init )
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
      }

      piece_Ch.move(pos_fin._1,pos_fin._2,Ksparov.board)

    }

    override def getmove : Unit = {
      if (!list_of_moves.isEmpty){
        parse_word(list_of_moves.head)
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

    infos(0) = filename

    var i = 1

    for (lines <- Source.fromFile(Constants.save_path + filename + ".pgn").getLines()){
      if (i<= 7){
        matchtag.findFirstIn(lines) match {
          case Some(s) =>
            infos(i) = s
            i = i + 1
          case None => ()
        }
      }
      else{
        matchtag.findFirstIn(lines) match {
          case Some(s) => println("pb")
          case None =>
            val array_of_words = lines.split(' ')
            for (i <- 0 to array_of_words.length -1) {
              array_of_words(i) match {
                case tourtag(_*) => ()
                case "" => ()
                case _ => list_of_moves = array_of_words(i) :: list_of_moves
              }
            }
        }
      }
    }
    list_of_moves = list_of_moves.reverse
  }


}
