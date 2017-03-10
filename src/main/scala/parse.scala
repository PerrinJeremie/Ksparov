import java.io._
import sys.process._


object Save_constants {

  var can_overwrite = false
  var curr_save = "coucou"

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
  var writer = new PrintWriter (new File (Constants.resources_path + Constants.save_path + Save_constants.curr_save))

  def is_valid : Boolean = {
    var res : String = "ls " + Constants.resources_path + Constants.save_path !!;
    println (res)
    return true
  }

  def main(argv : Array[String]) {
    var b = is_valid
  }
}

