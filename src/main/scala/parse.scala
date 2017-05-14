import java.io._
import scala.io.Source
import sys.process._
import scala.language.postfixOps
import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.PrintWriter
import scala.util.matching.Regex
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import sys.process._
import scala.language.postfixOps
import java.awt.image.BufferedImage  
import java.awt.Image                                                                                            
import javax.imageio.ImageIO
import java.awt.{Graphics2D,Color,Font,BasicStroke} 
import javax.swing.JFileChooser     
import java.io.{File,FileInputStream,FileOutputStream}



/** Object which regroups the functions and values associated with saving games.*/
object Save {


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

  /** tests the validity of the string argument as a filename for the save : -2 if too long, -1 if already exists. 
  *
  * @param s The string we will check if it could be a filename
  * @return 0 if ok, -1 if the name il already existing, -2 if the filename is too long
  */
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

  /** To be called in the getmove method of a Player right before the applied move, loads part of curr_move. 
  *
  * @param pos_piece The index of the piece in the current game board
  * @param p The pair of coordinates where the piece will be after the move 
  */  
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

  /** Called in the promotion method of object Ksparov, modifies curr_move. 
      If add2_happened = true, replaces the head of list_of_moves by the new value of curr_move. 
  * @param s The name of the piece selected for the promotion
  * @param b True if the king of the oppponent player is attacked
  */
  def add_prom_to_move (s : String, b : Boolean) : Unit = {
    val piece_prom : String =
      s match {
        case "Knight" => "N"
        case "Bishop" => "B"
        case "Rook" => "R"
        case "Queen" => "Q"
      }
    curr_move = (curr_move._1, true, piece_prom, curr_move._4, curr_move._5, curr_move._6 || b, curr_move._7, curr_move._8)
    if (add2_happened) {
      list_of_moves = curr_move :: list_of_moves.tail
    }
  }

  /** Writes the tag of the .PGN file.
  *
  * @param writer The PrintWriter we will write in
  * @param event The name of the event of the game
  * @param site The site of the game
  * @param date The date the game took place
  * @param round The round of the game 
  * @param white The name of the white player
  * @param black The name of the black player
  * @param result The result of the game
  */
  def write_tags (writer : PrintWriter, event : String, site : String, date : String, round : String, white : String, black : String, result : String) : Unit = {
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

  /** Translates (i,j) into (aj : String) where aj is the board representation of the position (x,y).
  *
  * @param p The position we will convert
  * @return The string of the coordinates
  */
  def pos_to_PGN (p : (Int,Int)) : String = {
    (97 + p._1).toChar + (p._2 + 1).toString
  }

  /** Write all moves in the previously opened file 
  *
  * @param writer The PrintWriter we will write into
  */
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

  /** Returns 0 if all went well. Opens a file, writes tags, then moves, then result into said file.
  *
  * @param s The name of the file we will write in
  * @param event The name of the event of the game
  * @param site The site of the game
  * @param date The date the game took place
  * @param round The round of the game 
  * @param white The name of the white player
  * @param black The name of the black player
  * @param result The result of the game
  * @return 0 if the save went well, -1 otherwise 
  */
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
    Load.piece_Ch  = new Pawn(0,-1,-1,0)
    pos_init = (0,0)
    pos_fin  = (0,0)
  }

  /** Predicates true if char is not a blank space character 
  *
  * @param c The character investigated
  * @return True if the character is not a blank space
  */
  def pnotspace (c : Char) : Boolean = {
    return ( c != ' ' && c != '\t' && c != '\n')
  }

  /** Predicates true if char is not " nor [  
  *
  * @param c The character investigated
  * @return True if the character is not " nor [
  */
  def pnotspec (c : Char) : Boolean = {
    return (c != '\"' && c != '[')
  }


    /** Finds all information related to the move and plays the move once done 
    *
    * @param s The move in a string 
    * @return True the string is a move, False if we did not manage to parse the string
    */
    def parse_word (s : String) : Boolean = {

      s match {
        case resulttag(_*) =>
          finalresult = s
          Ksparov.check_game_status(Ksparov.curr_game.curr_player)
          if (! (s == "*") ) {Ksparov.curr_game.game_won = true; return true} else { return false }
        case _ => ()
      }

      var w = s + "$$$$$"

      /* Recherche du type de pièce */
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
        Load.piece_Ch = Ksparov.curr_game.board.filter(pcolumn)(0)
        w = w.substring(1, w.length)
      }
      else{
        if( is_y_axis(w(0)) && (w(1) == 'x' || is_x_axis(w(1)))){
          pos_init = (0,w(0).toInt - 49)
          Load.piece_Ch = Ksparov.curr_game.board.filter(pline)(0)
          w = w.substring(1,w.length)
        }
        else{
          if (is_x_axis(w(0)) && is_y_axis(w(1)) && (w(2) == 'x' || is_x_axis(w(2)))){
            pos_init = ( w(0).toInt - 97, w(1).toInt - 49)
            Load.piece_Ch =  Ksparov.curr_game.board.filter(pexactpos)(0)
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

      if(Load.piece_Ch.coords == (-1,-1)){
        Load.piece_Ch = Ksparov.curr_game.board.filter(pcangoto)(0)
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

      Save.add_move1(Ksparov.curr_game.board.indexOf(Load.piece_Ch),pos_fin)
      Load.piece_Ch.move(pos_fin._1,pos_fin._2,Ksparov.curr_game.board)
      Save.add_move2
      return true

    }
/** Predicates if char is [a-h] 
    *
    * @param c The character investigated
    * @return True if the character is between 'a' and 'h'
    */
    def is_x_axis (c : Char) : Boolean = {
      return xaxistag.findFirstIn(c.toString) match {
        case Some(s) => true
        case None => false
      }
    }

    /** Predicates if char is [1-8]
    *
    * @param c The character investigated
    * @return True if the character is between '1' and '8'
    */
    def is_y_axis (c : Char) : Boolean = {
      yaxistag.findFirstIn(c.toString) match{
        case Some(s) => true
        case None => false
      }
    }

    /** Predicates if piece is in column of pos_init 
    *
    * @param P The piece investigated
    * @return True if the piece is in the column of pos_init
    */
    def pcolumn (P : Piece) : Boolean = {
      return ((P.name == piece) && (P.player == Ksparov.curr_game.curr_player) && P.pos_x == pos_init._1)
    }

    /** Predicates if piece is in pos_init's line
    *
    * @param P The piece investigated
    * @return True if the piece is in the line of pos_init
    */
    def pline (P : Piece) : Boolean = {
      return (P.name == piece && P.player == Ksparov.curr_game.curr_player && P.pos_y == pos_init._2)
    }

    /** Predicates if piece is in pos_init position 
    *
    * @param P The piece investigated
    * @return True if the piece is at the position of pos_init
    */
    def pexactpos (P : Piece) : Boolean = {
      return (P.name == piece && P.player == Ksparov.curr_game.curr_player && P.coords == pos_init)
    }

    /** Predicates if piece can go to pos_fin position 
    *
    * @param P The piece investigated
    * @return True if the piece can reach the pos_fin position
    */ 
    def pcangoto (P :Piece): Boolean = {
      return (P.name == piece && (P.player == Ksparov.curr_game.curr_player) && P.pre_move(pos_fin._1,pos_fin._2,Ksparov.curr_game.board)._1)
    }

  /** Represents a robot player which can read from Load.list_of_moves a move and play it, it is he who understands PGN move notation 
  *
  * @param n The id of the player
  */
  class Reproducer (n : Int) extends Player (n : Int) {

    /** It is a robot */
    ai = true

    

    /** the getmove method applies parse_word and prints board*/
    override def getmove : Unit = {
      if (!list_of_moves.isEmpty){
        reset_when_parsing
        try {
          Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = parse_word(list_of_moves.head)
        } catch {
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

  /** Reads a move and adds it to list_of_moves 
  *
  * @param lines The move in a string
  */
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

  /** Reads a line and treats it differently depending of PGN requirements 
  *
  * @param lines The line we will read
  */
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

  /** Reads from file. filename is the name of the file without the .pgn extension. 
      It escapes variations and comments and linefeed character.
  *
  * @param filename The name of the file we will read
  * @param full_name True is the file is given with its full path
  */
  def get_list_move_from_file (filename : String, full_name : Boolean) : Unit = {

    infos = Map()
    infos += ("filename" -> filename)

    var i = 1
    var texte_entier = ""

    // If we have the full path, we do not do anything, else we complete the path with path to the Saves folder 
    if (full_name) {
      for (lines <- Source.fromFile(filename).getLines()) {
        texte_entier += lines + "\n"
      }
    } else {
      for (lines <- Source.fromFile(Display.save_path + filename + ".pgn").getLines()) {
        texte_entier += lines + "\n"
      }
    }

    var tableau = texte_entier.split("\\(|\\)").zipWithIndex.filter(x => x._2 % 2 == 0).map(_._1)
    texte_entier = tableau.mkString.split("\\{|\\}").zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    texte_entier.split('\n').iterator.foreach(read_line)
    list_of_moves = list_of_moves.reverse
  }
}


/** This object is used to draw the menu for loading games. */
object DrawCharge {

    /** Defines a JFileChooser method to get PGN in a file explorer */
    class ChoosePGN {
        /** Method that runs the File Chooser itself */
        def run {
            var chooser = new JFileChooser()
            var pgn_filter = new javax.swing.filechooser.FileNameExtensionFilter ("PGN", "pgn")
            chooser.setCurrentDirectory(new java.io.File("."))
            chooser.setDialogTitle("Choississez un fichier PGN à charger")
            chooser.setFileFilter(pgn_filter)
            if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                file_chosen.path = chooser.getSelectedFile.toString
                file_chosen.text = chooser.getSelectedFile.getName()
                val src = chooser.getSelectedFile
                val dest = new File("src/main/resources/Saves/" + file_chosen.text)
                // If the file already exists in Saves, we do not copy it to the folder
                if (! dest.exists) {
                  "touch src/main/resources/Saves/" + file_chosen.text !!;
                  new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src) getChannel, 0, Long.MaxValue)
                }
                Ksparov.frame.contents = new DrawCharge.Dcharge
            } else {
                file_chosen.text = "Aucun fichier choisi"
                Ksparov.frame.contents = new DrawCharge.Dcharge
            }
        }
    }

    /** Button to launch the file explorer */
    class FileChooserButton extends PrettyBigButton {
        action = Action ("Choisir un fichier sur l'ordinateur") {
            var file_chooser = new ChoosePGN
            file_chooser.run
        }
    }

    /** Label that display the name of the file chosen by the file explorer */
    var file_chosen = new Label ("Aucun fichier choisi") {
        /** Contain the entire path, whereas text only contains the file name */
        var path = ""
        preferredSize = Display.dim_message_drawer
    }

  /** Returns the string passed in argument without the 4 last characters. 
  *
  * @param s The string that should be cut
  * @return The argument minus its four last characters
  */
    def shorten(s : String) : String = {
        return s.substring(Display.save_path.length, s.length - 4)
    }

    /** Predicates if a string contains at least one space character. 
    *
    * @param s The inquired string 
    * @return True if the string contains a space caracter
    */
    def pred_nospace(s : String) : Boolean = {
      return !s.contains (' ')
    }

    /** Stores the result of the loaded game. */
    var result : String = "";
    /** Stores the list of games found in src/main/resources/Saves folder. */
    var listgame : List[String] = List("")
    /** The component used to display the listgame value. */
    var scroll = new ComboBox(listgame)
    /** True if and only if listgame is empty after trying to load saved games. i.e no game in src/main/resources/Saves folder.*/
    var list_empty = false
    /** A switch to know what to print depending on the emptiness of listgame. */
    var text_label = ""

    /** Defines the value of result with the list of pgn files found in src/main/resources/Saves,
    and actualizes other value depending on the fact that there is files or not. */
    def define_listgame {
        result = "find src/main/resources/Saves/ -regex .*[.]pgn " !!;
        if (result.length == 0) {
          list_empty = true
          text_label = "nosaves"
        } else {
          list_empty = false
        DrawCharge.listgame = result.split('\n').map(shorten).filter(pred_nospace).toList.sortBy(_.toLowerCase)
        DrawCharge.scroll = new ComboBox(listgame) {
          font = Display.text_font
        }
        }
    }

    /** Defines the button for the loading menu. 
    *
    * @param text Text display on the button
    * @param return_type Defines the action when the button is pressed
    * @param full_name True if the file is given by its full path, else the file is in the Saves folder
    */
    class Option (text : String, return_type : String, full_name : Boolean) extends PrettyBigButton {
    action = Action (text) {
      return_type match {
        case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
          Ksparov.frame.peer.setLocationRelativeTo(null)

        case "Game_begin" =>
                  Ksparov.curr_game = new Ksparov.Game (6, 1, false)
                  Load.list_of_moves = List()
                    if (full_name) {
                        if (file_chosen.text != "Aucun fichier choisi") {
                            Load.get_list_move_from_file(file_chosen.path, true)
                            Ksparov.init_game(6)
                            Ksparov.frame.contents = new DrawBoard.Board
                            Ksparov.frame.peer.setLocationRelativeTo(null)
                        }
                    } else {
                        Load.get_list_move_from_file(scroll.item, false)
                        Ksparov.init_game(6)
                        Ksparov.frame.contents = new DrawBoard.Board
                        Ksparov.frame.peer.setLocationRelativeTo(null)
                    }

              case "Game_end" =>
                    Ksparov.curr_game = new Ksparov.Game (7, 1, false)
                    Load.list_of_moves = List()
                    if (full_name) {
                        if (file_chosen.text != "Aucun fichier choisi") {
                            Load.get_list_move_from_file(file_chosen.path, true)
                            Ksparov.init_game(7)
                            Ksparov.play_move
                            Ksparov.frame.contents = new DrawBoard.Board
                            Ksparov.frame.peer.setLocationRelativeTo(null)
                        }
                    } else {
                        Load.get_list_move_from_file(scroll.item, false)
                        Ksparov.init_game(7)
                        Ksparov.play_move
                        Ksparov.frame.contents = new DrawBoard.Board
                        Ksparov.frame.peer.setLocationRelativeTo(null)
                    }

              case "Delete" =>
                    val res_ = ("rm " + Display.save_path + scroll.item + ".pgn") !!;
                    define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
      }
    }
  }

  /** Defines the labels used in the loading menu, with the characteristics defined in Display. 
  *
  * @param text The text of the label
  */
  class PrettyLabel (text : String) extends PrettyBigLabel (text) {
    background = new Color (200, 200, 200)
    opaque = true
  }

  /** The main grid of the loading menu with everything. */
  class CenterGrid extends BorderPanel {
        layout (new GridPanel (10,1) {
    for (i <- 0 to 9) {
        i match {
        case 1 =>
                    if (list_empty) {
                        contents += new PrettyLabel ("<html><div style='text-align : center;'>Aucune sauvegarde n'est disponible !</html>")
                    } else {
                      contents += new PrettyLabel ("<html><div style='text-align : center;'>Quelle sauvegarde voulez-vous charger ?</html>")
                    }

          case 2 =>
                    if (!list_empty) {
                      contents += scroll}
                  else {
                      contents += new BackgroundCase (1,5)
                    }

          case 4 =>
                    if (!list_empty) {
                      contents += new Option ("<html><div style='text-align : center;'>Charger la partie ci-dessus <br> depuis le début</html>", "Game_begin", false)
                    } else {
                      contents += new PrettyLabel ("<html><div style='text-align : center;'> Chessgames.com pour télécharger des parties !</html>")
                    }

                case 6 =>
                    if (!list_empty) {
                      contents += new Option ("<html><div style='text-align : center;'>Charger la partie ci-dessus <br> à la fin</html>", "Game_end", false)
                    } else {
                      contents += new BackgroundCase (1,5)
                    }

                case 8 =>
                    if (!list_empty) {
                        contents += new Option  ("<html><div style='text-align : center;'>Supprimer la partie</html>", "Delete", true)
                    } else {
                        contents += new BackgroundCase (1,5)
                    }

          case _ => contents += new BackgroundCase (1, 5)
        }
    }
        }) = West

        layout (new BackgroundCase (10, 1)) = Center

        layout (new GridPanel (10, 1) {
            for (i <- 0 to 9) {
            i match {
                case 1 => contents += new FileChooserButton

                case 2 => contents += file_chosen

                case 4 => contents += new Option ("<html><div style='text-align : center;'>Charger la partie ci-dessus <br> depuis le début</html>", "Game_begin", true)
                    
                case 6 => contents += new Option ("<html><div style='text-align : center;'>Charger la partie ci-dessus <br> à la fin</html>", "Game_end", true)

                case 8 => contents += new Option ("<html><div style='text-align : center;'>Revenir au menu</html>", "Menu", false)

                case _ => contents += new BackgroundCase (1, 5)
            }
        }
        }) = East
  }

  /** The final menu with the central grid and background columns on each sides of it. */
  class Dcharge extends BorderPanel {
    define_listgame
      layout (new BackgroundCase (12,1)) = East
      layout (new BackgroundCase (12,1)) = West
      layout (new CenterGrid) = Center
  }
}

/** Draws the menus used to save games.*/
object DrawSave {

    /** A text field that stores a certain information conserning the save, to be filled by the user. 
    *
    * @param default The default string display in the textfield
    * @param col The columns0 argument of a TextField*/
  class SaveArgument (default : String, col : Int) extends TextField (default, col) {
    border = new javax.swing.border.LineBorder (Color.black, 2)
        preferredSize = Display.dim_message_drawer
    minimumSize = Display.dim_message_drawer
    maximumSize = Display.dim_message_drawer
    font = Display.text_font
    foreground = Color.black
    listenTo(mouse.clicks)
      reactions += {
          case _ : event.MouseClicked =>
            foreground = Color.black
            text = ""
      }
    }

    /** The label that indicates what the texfield bellow stands for
    *
    * @param str Text display on the label
    */
    class SaveLabel (str : String) extends PrettyBigLabel (str) {
    preferredSize = Display.dim_message_drawer
    minimumSize = Display.dim_message_drawer
    maximumSize = Display.dim_message_drawer
    background = new Color (200, 200, 200)
    opaque = true
    }

    /** Returns the result tag associated with (player, game_won, game_null). 
    *
    * @param p Player id 
    * @param gw True if the game is won
    * @param gn True if the game is nulle 
    * @return The result tag for the game
    */
    def resultgame (p : Int, gw : Boolean, gn : Boolean) : String = {
      (p, gw, gn) match {
        case (_, _, true) => "1/2-1/2"
        case (1, true, _) => "1-0"
        case (0, true, _) => "0-1"
        case _ => "*"
      }
    }

    /** A text field, it's name explains it all. */
  val textFileName = new SaveArgument ("", 0)
    /** A text field, it's name explains it all. */
  val textEvent = new SaveArgument ("Ksparov Tournament", 0)
    /** A text field, it's name explains it all. */
  val textSite = new SaveArgument ("Ksparov Software", 0)
    /** A text field, it's name explains it all. */
  val textDate = new SaveArgument (new SimpleDateFormat("yyyy.MM.dd").format(Calendar.getInstance().getTime()), 0)
    /** A text field, it's name explains it all. */
  val textRound = new SaveArgument ("Ronde numéro ", 0)
    /** A text field, it's name explains it all. */
  val textWhite = new SaveArgument ("Garry Kasparov", 0)
    /** A text field, it's name explains it all. */
  val textBlack = new SaveArgument ("Bobby Fischer", 0)

    /** Button component which saves game and brings you back where the retur_type tells you to
    *
    * @param text The text display on the button
    * @param return_type Where you want to come back on the click of the button, defines the action of the button
    */
  class ComeBack (text : String, return_type : String) extends PrettyBigButton {
    preferredSize = Display.dim_message_drawer
    minimumSize = Display.dim_message_drawer
    maximumSize = Display.dim_message_drawer
    action = Action (text) {
      Save.write_to_file(textFileName.text.replaceAllLiterally(" ","_"), textEvent.text, textSite.text, textDate.text, textRound.text, textWhite.text, textBlack.text) match {
              case 0 =>
          textFileName.text = ""
          return_type match {
            case "Menu" => 
              Ksparov.frame.contents = new DrawMenu.Menu
              Ksparov.curr_game.gnuthread_in_life = false
              Ksparov.curr_game.send_to_gnuchess.join
              Ksparov.curr_game.listen_to_gnuchess.join
              Ksparov.frame.peer.setLocationRelativeTo(null)
            case "Game" => 
              Ksparov.curr_game.timer = new Time.TimeThread
              Ksparov.curr_game.ai_move = new AIMoveThread
              Ksparov.curr_game.thread_in_life = true
              Ksparov.frame.contents = new DrawBoard.Board
              Ksparov.curr_game.timer.start
              Ksparov.curr_game.ai_move.start
            case "Quit" => 
              Ksparov.curr_game.gnuthread_in_life = false
              Ksparov.curr_game.send_to_gnuchess.join
              Ksparov.curr_game.listen_to_gnuchess.join
              Ksparov.frame.dispose()
        }
              case -1 =>
                textFileName.foreground = Color.red
          textFileName.text = "SAUVEGARDE DEJA EXISTANTE"
                case -2 =>
                  textFileName.text = " 30 Caractères Max. "
                case -3 =>
                    textDate.foreground = Color.red
                    textDate.text = "FORMAT AAAA.MM.JJ A RESPECTER"
          }
    }
  }

    /** Button component brings you back to the game without saving. */
  class CancelButton extends PrettyBigButton {
    preferredSize = Display.dim_message_drawer
    minimumSize = Display.dim_message_drawer
    maximumSize = Display.dim_message_drawer
    action = Action ("Annuler") {
      Ksparov.curr_game.timer = new Time.TimeThread
        Ksparov.curr_game.ai_move = new AIMoveThread
        Ksparov.curr_game.thread_in_life = true
        Ksparov.frame.contents = new DrawBoard.Board
        Ksparov.curr_game.timer.start
        Ksparov.curr_game.ai_move.start
    }
  }

    /** Button switches between simple-save (only choice is filename), and advanced-save (all choices). 
    *
    * @param switch_type Defines which type of save you want to go to
    */
  class SwitchButton (switch_type : String) extends PrettyBigButton {
    preferredSize = Display.dim_message_drawer
    minimumSize = Display.dim_message_drawer
    maximumSize = Display.dim_message_drawer
    if (switch_type == "AdvancedSave") {
      action = Action ("<html><div style='text-align : center;'>Passer en mode<br>sauvegarde avancée</html>") {
        Ksparov.frame.contents = new DrawSave.AdvancedSave
        Ksparov.frame.peer.setLocationRelativeTo(null)
      }
    } else {
      action = Action ("<html><div style='text-align : center;'>Revenir à la<br>sauvegarde simple</html>") {
        Ksparov.frame.contents = new DrawSave.SimpleSave
        Ksparov.frame.peer.setLocationRelativeTo(null)
      }
    }
  }

    /** Left component part of the simple-save menu.*/
  class LeftSimpleGrid extends GridPanel (8, 1) {
    for (i <- 0 to 7) {
      i match {
          case 1 => contents += new SaveLabel ("<html><div style='text-align : center;'>Quelle nom donner à la sauvegarde (30 caractères max) ?</html>")
          case 2 => contents += textFileName
          case 4 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>revenir à la partie</html>", "Game")
          case 6 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>quitter</html>", "Quit")
          case _ => contents += new BackgroundCase (1, 5)
        }
    }
  }

    /** Right component part of the simple-save menu. */
  class RightSimpleGrid extends GridPanel (8, 1) {
    for(i <- 0 to 7) {
      i match {
        case 1 => contents += new SwitchButton ("AdvancedSave")
        case 4 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>revenir au menu principal</html>", "Menu")
        case 6 => contents += new CancelButton
        case _ => contents += new BackgroundCase (1, 5)
      }
    }
  }

    /** Left component part of the advanced-save menu.*/
  class LeftAdvancedGrid extends GridPanel (13, 1) {
    for (i <- 0 to 8) {
      i match {
        case 1 => contents += new SaveLabel ("<html><div style='text-align : center;'>Quelle nom donner à la sauvegarde (20 caractères max) ?</html>")
          contents += textFileName
        case 3 => contents += new SaveLabel ("<html><div style='text-align : center;'>Quel est l'évènement<br>de cette partie ? </html>")
          contents += textEvent
        case 5 => contents += new SaveLabel ("<html><div style='text-align : center;'>Où s'est déroulé<br> cet évènement ? </html>")
          contents += textSite
        case 7 => contents += new SaveLabel ("<html><div style='text-align : center;'>Qui joue les blancs ?</html>")
          contents += textWhite
        case _ => contents += new BackgroundCase (1, 5)
      }
    }
  }

    /** Center component part of the advanced-save menu.*/
  class CenterAdvancedGrid extends GridPanel (13, 1) {
    for (i <- 0 to 8) {
      i match {
        case 1 => contents += new BackgroundCase (1, 5)
          contents += new BackgroundCase (1, 5)
        case 3 => contents += new SaveLabel ("<html><div style='text-align : center;'>Quelle est la date<br>de cette partie ? </html>")
          contents += textDate
        case 5 => contents += new SaveLabel ("<html><div style='text-align : center;'>Cette partie joue<br> pour quelle ronde ? </html>")
          contents += textRound
        case 7 => contents += new SaveLabel ("<html><div style='text-align : center;'>Qui joue les noirs ?</html>")
          contents += textBlack
        case _ => contents += new BackgroundCase (1, 5)
      }
    }
  }

    /** Right component part of the advanced-save menu.*/
  class RightAdvancedGrid extends GridPanel (13, 1) {
    for (i <- 0 to 12){
      i match {
        case 1 => contents += new SwitchButton ("SimpleSave")
        case 5 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>quitter</html>", "Quit")
        case 7 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>revenir à la partie</html>", "Game")
        case 9 => contents += new ComeBack ("<html><div style='text-align : center;'>Sauvegarder et<br>revenir au menu principal</html>", "Menu")
        case 11 => contents += new CancelButton
        case _ => contents += new BackgroundCase (1, 5)
      }
    }
  }

    /** A panel that regroups all components of the simple-menu */
  class SimpleSave extends BorderPanel {
      layout (new BorderPanel {
        layout (new BackgroundCase (8,1)) = West
        layout (new LeftSimpleGrid) = East
        }) = West
      layout (new BackgroundCase (8,1)) = Center
      layout (new BorderPanel {
        layout (new RightSimpleGrid) = West
        layout (new BackgroundCase (8, 1)) = East
      }) = East
  }

    /** A panel which gather all components of the advanced-menu */
  class AdvancedSave extends BorderPanel {
    layout (new BorderPanel {
      layout (new BackgroundCase (13, 1)) = West
      layout (new LeftAdvancedGrid) = East
    }) = West
    layout (new BorderPanel {
      layout (new BackgroundCase (13, 1)) = West
      layout (new CenterAdvancedGrid) = East
    }) = Center
    layout (new BorderPanel {
      layout (new BackgroundCase (13, 1)) = West
      layout (new RightAdvancedGrid) = Center
      layout (new BackgroundCase (13, 1)) = East
    }) = East
  }
}
