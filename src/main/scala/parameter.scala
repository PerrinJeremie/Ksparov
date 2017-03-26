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

/** Contains all classes to draw parameters selections.
   Parameters are saved in src/main/resources/Parameters */
object DrawParameters {

  /** Defines button for the selection of the sub menu. */
  class SubMenuChoice (id : Int, current_menu : Boolean) extends Button {
    preferredSize = new Dimension (Display.base_size, Display.base_size)
    maximumSize = new Dimension (Display.base_size, Display.base_size)
    minimumSize = new Dimension (Display.base_size, Display.base_size)
    border = new javax.swing.border.LineBorder (Color.black, 2)
    action = new Action ("") {
      icon = new javax.swing.ImageIcon(Display.resources_path + "parameter_sub_menu" + id + ".png")
      background = new Color (0, 0, 0)
      /* The border is red for the selected sub-menu. */
      if (current_menu) {
        border = new javax.swing.border.LineBorder (Color.red, 2)
      }
      def apply = {
        Ksparov.frame.contents = new DrawParameters.SubMenus(id)
      }
    }
  }

  /** Defines the column with all buttons for sub-menu and background cases elsewhere. */
  class ChoiceColumn (height : Int, current_menu : Int) extends GridPanel (height, 2) {
    for (i <- 0 to height - 1) {
      for (j <- 0 to 1) {
        if (j == 0) {
          i match {
            case 1 => contents += new SubMenuChoice (1, 1 == current_menu)
            case 3 => contents += new SubMenuChoice (2, 2 == current_menu)
            case 5 => contents += new SubMenuChoice (3, 3 == current_menu) 
            case _ => contents += new BackgroundCase (1, 1)
          }
        } else {
          contents += new BackgroundCase (1, 1)
        }
      }
    }
  }

  /** Defines the button to apply parameters and come back to the main menu. */
  class ComeBack (sub_menu_id : Int) extends Button {
    font = Display.text_font
    preferredSize = new Dimension (Display.base_size * 9, Display.base_size)
    border = new javax.swing.border.LineBorder (Color.black, 2)
    action = Action("Appliquer les changements et revenir au menu principal") {
      sub_menu_id match {
        case 3 => 
          /** The regex used to check if time for every period is well formated. */
          val time_regex = """([\d][\d]):([0-5][0-9]):([0-5][0-9])""".r
          var correct_time = true
          for (i <- 0 to Time.nb_period - 1) {
            time_textfields(i).text match {
              case time_regex (_*) => 
                if (Time.hhmmss_to_int(time_textfields(i).text) == 0) {
                  time_textfields(i).text = "Doit être > 0"
                  correct_time = false
                }
              case _ => time_textfields(i).text = "hh:mm:ss" 
                correct_time = false
            }
          }
          if (correct_time) {
            Time.periods = new Array [Time.Period] (Time.nb_period)
            for (i <- 0 to Time.nb_period - 1) {
              Time.periods(i) = new Time.Period (Time.hhmmss_to_int(time_textfields(i).text), move_textfields(i).text.toInt, inc_textfields(i).text.toInt)
            }
            Parameters.write
            Ksparov.frame.contents = new DrawMenu.Menu
            Ksparov.frame.peer.setLocationRelativeTo(null)
          }
        case 2 =>
          Parameters.write
          Ksparov.frame.contents = new DrawMenu.Menu
          Ksparov.frame.peer.setLocationRelativeTo(null)
        case 1 => 
          Ksparov.frame.contents = new DrawMenu.Menu
          Ksparov.frame.peer.setLocationRelativeTo(null)
      }
    }
  }

  /* Buttons for the texture choice, the parameter defines which texture the button stands for. */
  class TextureOption (number : Int) extends Button {
    val pattern = new Regex("\\d")
    preferredSize = Display.dim_small
    /* The border is red for the actual parameter and black for other. The regural expression
       get the number in the texture path. */
    if ((pattern findAllIn Display.texture_path).mkString.toInt == number) {
      border = new javax.swing.border.LineBorder (Color.red, 2)
    } else {
      border = new javax.swing.border.LineBorder (Color.black, 2)
    }
    action = new Action("") {
      /* The action is different for other button because to have an icon on a button, you have to put in the action. */
      icon = new javax.swing.ImageIcon(Display.resources_path + "Texture_small_" + number.toString + ".png")
      def apply = {
        /* When the button is pushed, we write the new parameters in the src/main/resources/Parameters file.
           After doing this, we apply the new parameters to that choice is dynamic. */
        Display.texture_path = "Texture_small_" + number.toString + ".png"
        Parameters.write
        Parameters.apply
        Ksparov.frame.contents = new DrawParameters.SubMenus (1)
      }
    }
  }

  /* Button fot the piece type choice, excatly the same as before. */
  class PieceOption (number : Int) extends Button {
    val pattern = new Regex("\\d")
    preferredSize = Display.dim_small
    if ((pattern findAllIn Display.pieces_path).mkString.toInt == number) {
      border = new javax.swing.border.LineBorder (Color.red, 2)
    } else {
      border = new javax.swing.border.LineBorder (Color.black, 2)
    }
    action = new Action ("") {
      icon = new javax.swing.ImageIcon(Display.resources_path + "Pieces/" + number.toString + "/1/King.png")
      def apply {
        Display.pieces_path = "Pieces/" + number.toString + "/"
        Parameters.write
        Parameters.apply
        Ksparov.frame.contents = new DrawParameters.SubMenus (1)
      }
    }
  }

  /* The menu for texture : an alternance of background cases and texture option. */
  class TextureGrid extends GridPanel (1, 9) {
    for( i <- 1 to 9) {
      if (i % 2 == 0) {
        contents += new BackgroundCase (1, 1)
      } else {
        contents += new TextureOption (Math.round(i / 2) + 1)
      }
    }
  }

  /* The menu for pieces : an alternance of background cases and pieces option. */
  class PiecesGrid extends GridPanel (1, 9) {
    for( i <- 1 to 9) {
      if (i % 2 == 0 || i > 4) {
        contents += new BackgroundCase (1, 1)
      } else {
        contents += new PieceOption (Math.round(i / 2) + 1)
      }
    }
  }

  class ChoiceMessage (text : String) extends Label (text) {
    font = Display.text_font
    border = new javax.swing.border.LineBorder (Color.black, 2)
    background = new Color (200, 200, 200)
    opaque = true
  }

  class DisplaySubMenu extends BorderPanel {
    layout (new BorderPanel {
      layout (new BackgroundCase (9, 1)) = West
      layout (new ChoiceColumn (9, 1)) = East
    }) = West

    layout (new GridPanel (9, 1) {
      contents += new BackgroundCase (1, 9)
      contents += new ChoiceMessage ("Choissisez le fond")
      contents += new TextureGrid
      contents += new BackgroundCase (1, 9)
      contents += new ChoiceMessage ("Choissisez le type de pièces")
      contents += new PiecesGrid
      contents += new BackgroundCase (1, 9)
      contents += new ComeBack (1)
      contents += new BackgroundCase (1, 9)
    }) = Center

    layout (new BackgroundCase (9, 1)) = East
  }

  class Increment (variable : String, sub_menu_id : Int) extends GridPanel (1, 3) {
    class IncButton (sign : String) extends Button {
      preferredSize = Display.dim_small
      maximumSize = Display.dim_small
      minimumSize = Display.dim_small
      font = Display.text_font
      action = Action (sign) {
        if (sign == "-") {
          variable match {
            case "ai_speed" => Parameters.ai_speed = math.max (0, Parameters.ai_speed - 100)
            case "nb_alice_board" => Parameters.nb_alice_board = math.max (2, Parameters.nb_alice_board - 1)
            case "nb_period" => Time.nb_period = math.max (0, Time.nb_period - 1)
              if (Time.nb_period == 0) {
                Time.clock_available = false
              } else {
                Time.clock_available = true
              }
          }
        } else {
          variable match {
            case "ai_speed" => Parameters.ai_speed += 100
            case "nb_alice_board" => Parameters.nb_alice_board += 1
            case "nb_period" => Time.nb_period += 1
              if (Time.nb_period > 0) {
                Time.clock_available = true
              }
          }
        }
        Ksparov.frame.contents = new DrawParameters.SubMenus (sub_menu_id)
      }
    }

    contents += new IncButton ("-")
    contents += new Label {
      preferredSize = Display.dim_small
      variable match {
        case "ai_speed" => text = Parameters.ai_speed.toString
        case "nb_alice_board" => text = Parameters.nb_alice_board.toString
        case "nb_period" => text = Time.nb_period.toString
      }
    }
    contents += new IncButton ("+")
  }

  class SpeedAI extends BorderPanel {
    layout (new BackgroundCase (1, 1)) = West

    layout (new GridPanel (1, 2) {
      contents += new Label {
        preferredSize = Display.dim_big
        font = Display.text_font
        text = "<html><div style='text-align : center;'>Vitesse de jeu de l'IA<br>en milliseconde</html>"
      }
      contents += new Increment ("ai_speed", 2)
    }) = Center

    layout (new BackgroundCase (1, 2)) = East
  }

  class NbAliceBoard extends BorderPanel {
    layout (new BackgroundCase (1, 1)) = West

    layout (new GridPanel (1, 2) {
      contents += new Label {
        preferredSize = Display.dim_big
        font = Display.text_font
        text = "<html><div style='text-align : center;'>Nombre de plateau<br>de jeu d'Alice</html>"
      }
      contents += new Increment ("nb_alice_board", 2)
    }) = Center

    layout (new BackgroundCase (1, 2)) = East
  }

  class PlayabilitySubMenu extends BorderPanel {
    layout (new BorderPanel {
      layout (new BackgroundCase (7, 1)) = West
      layout (new ChoiceColumn (7, 2)) = East
    }) = West
  
    layout (new GridPanel (7, 1) {
      contents += new BackgroundCase (1, 9)
      contents += new SpeedAI
      contents += new BackgroundCase (1, 9)
      contents += new NbAliceBoard
      contents += new BackgroundCase (1, 9)
      contents += new ComeBack (2)
      contents += new BackgroundCase (1, 9)
    }) = Center

    layout (new BackgroundCase (7, 1)) = East 
  }

  var time_textfields = new Array [TextField] (Time.nb_period)
  var move_textfields = new Array [TextField] (Time.nb_period)
  var inc_textfields = new Array [TextField] (Time.nb_period)

  class NbPeriodChoice extends BorderPanel {
    layout (new BackgroundCase (1, 2)) = West

    layout (new GridPanel (1, 2) {
      contents += new Label {
        preferredSize = Display.dim_big
        font = Display.text_font
        text = "Nombre de période"
      }
      contents += new Increment ("nb_period", 3)
    }) = Center

    layout (new BackgroundCase (1, 2)) = East
  }

  class PeriodOptions (id : Int) extends BorderPanel {
    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Durée de la<br>période au<br>format hh:mm:ss</html>") {
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (time_textfields (id)) = East
    }) = West

    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Nombre de coup<br>de la période</html>") {
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (move_textfields (id)) = East
    }) = Center

    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Incrément après<br>un coup<br>en seconde</html>") {
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (inc_textfields (id)) = Center
    }) = East
  }

  class Periods (nb_period : Int) extends GridPanel (2 * nb_period + 1, 1) {
    for (i <- 0 to 2 * nb_period) {
      if (i % 2 == 0) {
        contents += new BackgroundCase (1, 10)
      } else {
        contents += new PeriodOptions (i / 2)
      }
    }
  }

  class ClockSubMenu (nb_period : Int) extends BorderPanel {
    time_textfields = new Array [TextField] (nb_period)
    move_textfields = new Array [TextField] (nb_period)
    inc_textfields = new Array [TextField] (nb_period)

    for (i <- 0 to nb_period - 1) {
      time_textfields (i) = new TextField {
        font = Display.text_font
        try {
          text = Time.int_to_hhmmss(Time.periods(i).time)
        } catch {
          case _ : Throwable => text = "00:00:00"
        }
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
        listenTo(keys)
        reactions += {
            case e : KeyTyped => if (!e.char.isDigit && e.char != ':') {e.consume}     
        }
      }
      move_textfields (i) = new TextField {
        font = Display.text_font
        try {
          text = Time.periods(i).nb_move.toString
        } catch {
          case _ : Throwable => text = "0"
        }
        preferredSize = Display.dim_small
        listenTo(keys)
        reactions += {
            case e: KeyTyped => if (!e.char.isDigit) {e.consume}     
        }
      }
      inc_textfields (i) = new TextField {
        font = Display.text_font
        try {
          text = Time.periods(i).inc.toString
        } catch {
          case _ : Throwable => text = "0"
        }
        preferredSize = Display.dim_small
        listenTo(keys)
        reactions += {
            case e: KeyTyped => if (!e.char.isDigit) {e.consume}     
        }
      }
    }

    layout (new BorderPanel {
      layout (new BackgroundCase (math.max (2 * nb_period + 5, 7), 1)) = West
      layout (new ChoiceColumn (math.max (2 * nb_period + 5, 7), 3)) = East
    }) = West

    layout (new BorderPanel {
      layout (new BorderPanel {
        layout (new BackgroundCase (1, 10)) = North
        layout (new NbPeriodChoice) = South
      }) = North
      if (Time.nb_period == 0) {
        layout (new BackgroundCase (3, 10)) = Center
      } else {
        layout (new Periods (nb_period)) = Center
      }
      layout (new BorderPanel {
        layout (new ComeBack (3)) = North
        layout (new BackgroundCase (1, 10)) = South
      }) = South
    }) = Center

    layout (new BackgroundCase (math.max (2 * nb_period + 5, 7), 1)) = East
  }

  /* The final menu with the texture choice first then the piece choice and finally a come back button. */
  class SubMenus (id : Int) extends GridPanel (1, 1) {
    id match {
      case 1 => contents += new DisplaySubMenu
      case 2 => contents += new PlayabilitySubMenu
      case 3 => contents += new ClockSubMenu (Time.nb_period)
    }
  }
}