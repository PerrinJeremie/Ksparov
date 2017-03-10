import java.io._

object Save_constants {

  var can_overwrite = false
  var curr_save = ""

  def init : Unit = {
    val r = scala.util.Random
    can_overwrite = false
    curr_save = ""
    for( i <- 0 to 15){
      curr_save = curr_save + (r.nextInt(16)).toString
    }
    curr_save = curr_save + ".pgn"
  }

}

object Save{
  var writer = new PrintWriter (new File (Constants.ressources_path + Save_constants.curr_save))
  def init : Unit = {
    writer = new PrintWriter (new 
  }
}

