import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.matching.Regex
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar

object Display {

  def apply_resolution {

    /* Defining the dimension and numbers of the cases */
    var resolution = java.awt.Toolkit.getDefaultToolkit().getScreenSize();

    if (resolution.getHeight < 1000.0) {
      dim_path = "Min/"
      base_size = 50
      dim_small = new Dimension (50, 50)
      dim_big = new Dimension (150, 50)
      dim_message_drawer = new Dimension (250, 50)
      text_font = new Font ("Gill Sans Cyr MT", 1, 11)
      num_dead_font = new Font("Arial", 0, 20)
    } else {
      if (resolution.getHeight > 1100.0) {
        dim_path = "Jeroboam/"
        base_size = 90
        dim_small = new Dimension (90, 90)
        dim_big = new Dimension (270, 90)
        dim_message_drawer = new Dimension (450, 90)
        text_font = new Font ("Gill Sans Cyr MT", 1, 20)
        num_dead_font = new Font("Arial", 0, 30)
      } else {
        dim_path = "Max/"
      }
    }
  resources_path = "src/main/resources/" + dim_path
  }

  var dim_path = ""
  var base_size = 70
  var dim_small = new Dimension (base_size, base_size)
  var dim_big = new Dimension (base_size * 3, base_size)
  var dim_message_drawer = new Dimension (base_size * 5, base_size)

  var resources_path = "src/main/resources/Max"
  var pieces_path = ""
  var save_path = "src/main/resources/Saves/"
  var texture_path = ""

  var text_font = new Font ("Gill Sans Cyr MT", 1, 16)
  var text_color = Color.black
  var num_dead_font = new Font("Arial", 0, 25)
}

object Parameters {
  def apply = {
    var i = 0
    /* Initializing lines with empty strings. */
    var lines = new Array [String] (7)

    /* Reading each lines of the file. */
    for (line <- Source.fromFile("src/main/resources/Parameters").getLines) {
      lines (i) = line.toString
      i += 1
    }

    /* Updating variables. */
    Display.pieces_path = "Pieces/" + lines(1) + "/"
    Display.texture_path = "Texture_small_" + lines(2) + ".png"
    Parameters.ai_speed = lines(3).toInt
    Parameters.nb_alice_board = lines(4).toInt
    Time.nb_period = lines(5).toInt
    parse_period_string(lines(6))

    /* Defining the text color depending on the texture selected. */
    lines(2).toInt match {
      case 1 => Display.text_color = Color.white
      case 2 => Display.text_color = Color.white
      case 3 => Display.text_color = Color.black
      case 4 => Display.text_color = Color.black
      case 5 => Display.text_color = Color.red
    }
  }

  def write {
    val pattern = new Regex("\\d")
    var writer = new PrintWriter(new File ("src/main/resources/Parameters"))
    writer.write ("Lines of this file : 1 - Pieces, 2 - Texture, 3 - IA speed, 4 - Nb Alice board, 5 - Nb period, 6 - Each_period\n")
    writer.write ((pattern findAllIn Display.pieces_path).mkString + "\n")
    writer.write ((pattern findAllIn Display.texture_path).mkString (",") + "\n")
    writer.write (Parameters.ai_speed.toString + "\n")
    writer.write (Parameters.nb_alice_board.toString + "\n")
    writer.write (Time.nb_period.toString + "\n")
    writer.write (Parameters.periods_to_string(Time.periods))
    writer.close
  }

  def periods_to_string (periods_array : Array [Time.Period]) = {
    var result = ""
    if (periods_array.length > 0) {
      for (i <- 0 to periods_array.length - 1) {
       result += "(" + periods_array(i).time + "," + periods_array(i).nb_move + "," + periods_array(i).inc + ")"
     }
    } else {
      result = "0"
    }
    result
  }

  def parse_period_string (periods_string : String) {
    Time.periods = new Array [Time.Period] (Time.nb_period)
    if (periods_string == "0") {
      Time.clock_available = false
    } else {
      Time.clock_available = true
      val pattern =  """([0-9]+),([0-9]+),([0-9]+)""".r
      var i = 0
      periods_string.split("\\(|\\)").filter(s => s != "").iterator.foreach(x => x match { 
        case pattern(s1,s2,s3) => Time.periods(i) = new Time.Period (s1.toInt, s2.toInt, s3.toInt)
          i += 1
      })
    }
  }

  var ai_speed = 400
  var nb_alice_board = 2
  var nb_case_board = 8

}