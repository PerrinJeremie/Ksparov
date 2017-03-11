import java.io._
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

  def is_valid (s:String) : Boolean = {
    var res : String = ("ls " + Constants.resources_path + Constants.save_path) !!;
    return (res.indexOf(s) == -1)
  }

  def init : Unit = {
    var list_of_moves = List()
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
          if (move._2){
            writer.write( " =" + move._3)
          }
          writer.write( " " + move._4 + pos_to_PGN(move._7))
          if(move._5){
            writer.write("x")
          }
          writer.write(pos_to_PGN(move._8))
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
      val writer = new PrintWriter (new File (Constants.resources_path + Constants.save_path + s + ".pgn" ))
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
  var list_of_moves : List[Moves] = List()

  var event : String = "" 
  var site : String = ""
  var date : String = ""
  var white : String = ""
  var black : String = ""
  var result : String = "" 


}
