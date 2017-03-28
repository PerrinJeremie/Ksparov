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

/** Defines evrything that is linked with the display */
object Display {

  /** Adapts displays to the resolutions of the screen */
  def apply_resolution {

    /** The resolution of the screen */
    var resolution = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
    if (resolution.getHeight < 1000.0) {
      dim_path = "Min/"
      base_size = 50
      dim_small = new Dimension (50, 50)
      dim_big = new Dimension (150, 50)
      dim_message_drawer = new Dimension (250, 50)
      text_font = new Font ("Gill Sans Cyr MT", 1, 11)
      num_dead_font = new Font("Arial", 0, 20)
      para_clock_font = new Font ("Arial", 1, 10)
    } else {
      if (resolution.getHeight > 1100.0) {
        dim_path = "Jeroboam/"
        base_size = 90
        dim_small = new Dimension (90, 90)
        dim_big = new Dimension (270, 90)
        dim_message_drawer = new Dimension (450, 90)
        text_font = new Font ("Gill Sans Cyr MT", 1, 20)
        num_dead_font = new Font("Arial", 0, 30)
        para_clock_font = new Font ("Arial", 1, 18)
      } else {
        dim_path = "Max/"
      }
    }
  // Adapt the the resources_path to the resolution of the screen
  resources_path = "src/main/resources/" + dim_path
  }

  /** The path that is changes based on the resolution of the screen */
  var dim_path = ""
  /** Base size of labels and buttons which is adapt to the resolution */
  var base_size = 70
  /** The dimension for small item */
  var dim_small = new Dimension (base_size, base_size)
  /** The dimension for big items */
  var dim_big = new Dimension (base_size * 3, base_size)
  /** The dimension for the message_drawer of the board, also used for other items */
  var dim_message_drawer = new Dimension (base_size * 5, base_size)

  /** Path where resources are */
  var resources_path = "src/main/resources/Max"
  /** Path for the set of pieces chosen, defines by Parameters.apply */
  var pieces_path = ""
  /** Path for the saves */
  var save_path = "src/main/resources/Saves/"
  /** Path for the texture of the background, defines by Parameters.apply */
  var texture_path = ""

  /** Font for labels and buttons of Ksparov */
  var text_font = new Font ("Gill Sans Cyr MT", 1, 16)
  /** Color of the text which changes with the background  */
  var text_color = Color.black
  /** Font for the number of dead pieces */
  var num_dead_font = new Font ("Arial", 0, 25)
  /** Font for the parameters of clocks */
  var para_clock_font = new Font ("Arial", 1, 14)
}

/** Defines methods and variables for the parameter */
object Parameters {

  /** Reads parameters from the src/main/resources/Parameters file and apply it */
  def apply = {
    /** Index for the iterator */
    var i = 0


    /** Array with lines of the Parameters file */
    var lines = Source.fromFile("src/main/resources/Parameters").getLines.toArray

    // Updating variables.
    Display.pieces_path = "Pieces/" + lines(1) + "/"
    Display.texture_path = "Texture_small_" + lines(2) + ".png"
    Parameters.ai_speed = lines(3).toInt
    Parameters.nb_alice_board = lines(4).toInt
    Time.nb_period = lines(5).toInt
    // Parse the line that defines every period of the clock
    parse_period_string(lines(6))

    // Defining the text color depending on the texture selected.
    lines(2).toInt match {
      case 1 => Display.text_color = Color.white
      case 2 => Display.text_color = Color.white
      case 3 => Display.text_color = Color.black
      case 4 => Display.text_color = Color.black
      case 5 => Display.text_color = Color.red
    }
  }

  /** Write the parameters in the Parameter file */
  def write {
    val pattern = new Regex("\\d")
    var writer = new PrintWriter(new File ("src/main/resources/Parameters"))
    writer.write ("Lines of this file : 1 - Pieces, 2 - Texture, 3 - IA speed, 4 - Nb Alice board, 5 - Nb period, 6 - Each_period\n")
    writer.write ((pattern findAllIn Display.pieces_path).mkString + "\n")
    writer.write ((pattern findAllIn Display.texture_path).mkString (",") + "\n")
    writer.write (Parameters.ai_speed.toString + "\n")
    writer.write (Parameters.nb_alice_board.toString + "\n")
    writer.write (Time.nb_period.toString + "\n")
    // Transform periods in one string and write it in the file
    writer.write (Parameters.periods_to_string(Time.periods))
    writer.close
  }

  /** Convert an Array of perriod into one string 
  *
  * @param periods_array The array of period to be converted
  * @return The string corresponding to the periods given
  */
  def periods_to_string (periods_array : Array [Time.Period]) = {
    /** The result string */
    var result = ""
    if (periods_array.length > 0) {
      for (i <- 0 to periods_array.length - 1) {
       result += "(" + periods_array(i).time + "," + periods_array(i).nb_move + "," + periods_array(i).inc + ")"
     }
    } else {
      // If there is no period, the string only contains 0
      result = "0"
    }
    result
  }

  /** Parse the string of the periods to defines game periods 
  *
  * @param periods_string The string to be parsed
  */
  def parse_period_string (periods_string : String) {
    // Initializes the periods array
    Time.periods = new Array [Time.Period] (Time.nb_period)
    // If there is non periods, disables clocks
    if (periods_string == "0") {
      Time.clock_available = false
    } else {
      Time.clock_available = true
      /** Pattern to recognize a period definition */
      val pattern =  """([0-9]+),([0-9]+),([0-9]+)""".r
      /** Index variable for the iterator on each period */
      var i = 0
      // For each period in the string, fill the periods array with it
      periods_string.split("\\(|\\)").filter(s => s != "").iterator.foreach(x => x match { 
        case pattern(s1,s2,s3) => Time.periods(i) = new Time.Period (s1.toInt, s2.toInt, s3.toInt)
          i += 1
      })
    }
  }

  /** Waiting time for the AI to move */
  var ai_speed = 400
  /** Number of boards used in Alice mode */
  var nb_alice_board = 2
  /** Number of cases of a board */
  var nb_case_board = 8
}

/** Contains all classes to draw parameters selections. */
object DrawParameters {

  /** Defines button for the selection of the parameter sub menu. 
  *
  * @param id The id of the submenu
  * @param current_menu True if this menu is the one the user is in 
  */
  class SubMenuChoice (id : Int, current_menu : Boolean) extends Button {
    preferredSize = new Dimension (Display.base_size, Display.base_size)
    maximumSize = new Dimension (Display.base_size, Display.base_size)
    minimumSize = new Dimension (Display.base_size, Display.base_size)
    border = new javax.swing.border.LineBorder (Color.black, 2)
    action = new Action ("") {
      icon = new javax.swing.ImageIcon(Display.resources_path + "parameter_sub_menu" + id + ".png")
      background = new Color (0, 0, 0)
      // The border is red for the selected sub-menu.
      if (current_menu) {
        border = new javax.swing.border.LineBorder (Color.red, 2)
      }
      def apply = {
        Ksparov.frame.contents = new DrawParameters.SubMenus(id)
      }
    }
  }

  /** Defines the column with all buttons for sub-menu and background cases elsewhere
  *
  * @param height The height of the column
  * @param current_menu The id of the menu the user is in
  */
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

  /** Defines the button to apply parameters and come back to the main menu. 
  *
  * @param sub_menu_id The id of the submenu where this button is set
  */
  class ComeBack (sub_menu_id : Int) extends Button {
    font = Display.text_font
    preferredSize = new Dimension (Display.base_size * 9, Display.base_size)
    border = new javax.swing.border.LineBorder (Color.black, 2)
    action = Action("Appliquer les changements et revenir au menu principal") {
      // The action depends on the sub-menu we are in 
      sub_menu_id match {
        // The clock sub-menu
        case 3 => 
          /** The regex used to check if time for every period is well formated. */
          val time_regex = """([\d][\d]):([0-5][0-9]):([0-5][0-9])""".r
          /** True if the time entered is correct, false on the opposite */
          var correct_time = true
          for (i <- 0 to Time.nb_period - 1) {
            // Check if the time of the period match the regex
            time_textfields(i).text match {
              // If it is ok, check if the time is non zero, if not display the reason why we do not accept the time given
              case time_regex (_*) => 
                if (Time.hhmmss_to_int(time_textfields(i).text) == 0) {
                  time_textfields(i).text = "Doit être > 0"
                  correct_time = false
                }
              // If not, draw the reason why we do not accept the entry
              case _ => time_textfields(i).text = "hh:mm:ss" 
                correct_time = false
            }
          }
          // If all entries are correct, we adjust the periods array, we write the new parameters and we come back to the main menu
          if (correct_time) {
            Time.periods = new Array [Time.Period] (Time.nb_period)
            for (i <- 0 to Time.nb_period - 1) {
              Time.periods(i) = new Time.Period (Time.hhmmss_to_int(time_textfields(i).text), move_textfields(i).text.toInt, inc_textfields(i).text.toInt)
            }
            Parameters.write
            Ksparov.frame.contents = new DrawMenu.Menu
            Ksparov.frame.peer.setLocationRelativeTo(null)
          }
        // The playability sub-menu
        case 2 =>
          Parameters.write
          Ksparov.frame.contents = new DrawMenu.Menu
          Ksparov.frame.peer.setLocationRelativeTo(null)
        // Display sub-menu
        case 1 =>
          Parameters.write
          Ksparov.frame.contents = new DrawMenu.Menu
          Ksparov.frame.peer.setLocationRelativeTo(null)
      }
    }
  }

  /** Buttons for the texture choice, the parameter defines which texture the button stands for. 
  *
  * @param number The id of the texture drawn 
  */
  class TextureOption (number : Int) extends Button {
    preferredSize = Display.dim_small
    /* The border is red for the actual parameter and black for other. The regural expression
       get the number in the texture path. */
    if ((new Regex("\\d") findAllIn Display.texture_path).mkString.toInt == number) {
      border = new javax.swing.border.LineBorder (Color.red, 2)
    } else {
      border = new javax.swing.border.LineBorder (Color.black, 2)
    }
    action = new Action("") {
      // Defines the icon of the button
      icon = new javax.swing.ImageIcon(Display.resources_path + "Texture_small_" + number.toString + ".png")
      def apply = {
        // The selected texture is actualize in real time to have a dynamic display
        Display.texture_path = "Texture_small_" + number.toString + ".png"
        Ksparov.frame.contents = new DrawParameters.SubMenus (1)
      }
    }
  }

  /** Button for the piece type choice 
  *
  * @param number The id of the drawn piece option
  */
  class PieceOption (number : Int) extends Button {
    preferredSize = Display.dim_small
    /* The border is red for the actual parameter and black for other. The regural expression
       get the number in the texture path. */
    if ((new Regex("\\d") findAllIn Display.pieces_path).mkString.toInt == number) {
      border = new javax.swing.border.LineBorder (Color.red, 2)
    } else {
      border = new javax.swing.border.LineBorder (Color.black, 2)
    }
    action = new Action ("") {
      icon = new javax.swing.ImageIcon(Display.resources_path + "Pieces/" + number.toString + "/1/King.png")
      def apply {
        Display.pieces_path = "Pieces/" + number.toString + "/"
        Ksparov.frame.contents = new DrawParameters.SubMenus (1)
      }
    }
  }

  /** The menu for texture : an alternance of background cases and texture option. */
  class TextureGrid extends GridPanel (1, 9) {
    for (i <- 1 to 9) {
      if (i % 2 == 0) {
        contents += new BackgroundCase (1, 1)
      } else {
        contents += new TextureOption (Math.round(i / 2) + 1)
      }
    }
  }

  /** The menu for pieces : an alternance of background cases and pieces option. */
  class PiecesGrid extends GridPanel (1, 9) {
    for (i <- 1 to 9) {
      if (i % 2 == 0 || i > 4) {
        contents += new BackgroundCase (1, 1)
      } else {
        contents += new PieceOption (Math.round(i / 2) + 1)
      }
    }
  }

  /** Message to explain the choice that has to be done 
  *
  * @param text The message displayed by the label
  */
  class ChoiceMessage (text : String) extends Label (text) {
    font = Display.text_font
    border = new javax.swing.border.LineBorder (Color.black, 2)
    background = new Color (200, 200, 200)
    opaque = true
  }

  /** Final display sub-menu with all its items */
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

  /** Draw a label with a variable and one plus button and one minus button to increase or decrease the variable 
  * 
  * @param variable The variable that will be changed
  * @param sub_menu_id The id of the submenu the increment is in
  */
  class Increment (variable : String, sub_menu_id : Int) extends GridPanel (1, 3) {
    /** Button to increase or decrease a variable 
    *
    * @param sign The sign : - or + that defines the action of the button : increase or decrease
    */
    class IncButton (sign : String) extends Button {
      preferredSize = Display.dim_small
      maximumSize = Display.dim_small
      minimumSize = Display.dim_small
      font = Display.text_font
      action = Action (sign) {
        // The action depends on the sign of the button 
        if (sign == "-") {
          variable match {
            // The speed should not be under 0
            case "ai_speed" => Parameters.ai_speed = math.max (0, Parameters.ai_speed - 100)
            // The number of board for alice game is at least 2
            case "nb_alice_board" => Parameters.nb_alice_board = math.max (2, Parameters.nb_alice_board - 1)
            // The number of periods can not be under 0
            case "nb_period" => Time.nb_period = math.max (0, Time.nb_period - 1)
          }
        } else {
          variable match {
            case "ai_speed" => Parameters.ai_speed += 100
            case "nb_alice_board" => Parameters.nb_alice_board += 1
            case "nb_period" => Time.nb_period += 1
          }
        }
        Ksparov.frame.contents = new DrawParameters.SubMenus (sub_menu_id)
      }
    }

    // We define now the BorderPanel with the minus button, the label with the variable and the plus button
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

  /** Draw the selection of the speed of the AI */
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

  /** Draw the selection of the number of board in Alice mode */
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

  /** Draw the playibility sub menu */
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

  /** Textfield array for the selection of the time of a period */
  var time_textfields = new Array [TextField] (Time.nb_period)
  /** Textfield array for the selection of the number of move of a period */
  var move_textfields = new Array [TextField] (Time.nb_period)
  /** Textfield array for the selection of the increment of a period */
  var inc_textfields = new Array [TextField] (Time.nb_period)

  /** Draw the selection of the number of periods */
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

  /** Draw the selection for the parameters of a given period 
  *
  * @param id The id of the period, in fact the number of the period
  */
  class PeriodOptions (id : Int) extends BorderPanel {
    // Time of the period
    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Durée de la<br>période au<br>format hh:mm:ss</html>") {
        font = Display.para_clock_font
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (time_textfields (id)) = East
    }) = West

    // Number of move of the period
    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Nombre de coup<br>de la période</html>") {
        if (id == Time.nb_period - 1) {
          enabled = false
          text = "<html><div style='text-align : center;'>Dernière période, pas de mouvement"
        }
        font = Display.para_clock_font
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (move_textfields (id)) = East
    }) = Center

    // Increment of the period
    layout (new BorderPanel {
      layout (new Label ("<html><div style='text-align : center;'>Incrément après<br>un coup<br>en seconde</html>") {
        font = Display.para_clock_font
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
      }) = West
      layout (inc_textfields (id)) = Center
    }) = East
  }

  /** Draw the selection for every periods of the game 
  *
  * @param nb_period The number of periods to be drawn
  */
  class Periods (nb_period : Int) extends GridPanel (2 * nb_period + 1, 1) {
    for (i <- 0 to 2 * nb_period) {
      if (i % 2 == 0) {
        contents += new BackgroundCase (1, 10)
      } else {
        contents += new PeriodOptions (i / 2)
      }
    }
  }

  /** Draw the clock sub-menu 
  *
  * @param nb_period The number of period that will be drawn in the submenu
  */
  class ClockSubMenu (nb_period : Int) extends BorderPanel {
    // Actualize arrays with the current number of periods
    time_textfields = new Array [TextField] (nb_period)
    move_textfields = new Array [TextField] (nb_period)
    inc_textfields = new Array [TextField] (nb_period)

    // Fill arrays with corresponding textfiels
    for (i <- 0 to nb_period - 1) {
      time_textfields (i) = new TextField {
        font = Display.text_font
        try {
          // Try to draw the current value of the time of this period
          text = Time.int_to_hhmmss(Time.periods(i).time)
        } catch {
          // If it is not defined yet, initializes the textfield with the default value
          case _ : Throwable => text = "00:00:00"
        }
        preferredSize = new Dimension (Display.base_size * 2, Display.base_size)
        listenTo(keys)
        // Restrict the possible keys entered to digits or the caracter ':'
        reactions += {
            case e : KeyTyped => if (!e.char.isDigit && e.char != ':') {e.consume}     
        }
      }
      move_textfields (i) = new TextField {
        font = Display.text_font
        try {
          // Try to draw the current value of the number of moves of this period
          text = Time.periods(i).nb_move.toString
        } catch {
          // If it is not defined yet, initializes the textfield with the default value
          case _ : Throwable => text = "0"
        }
        if (i == nb_period - 1) {
          enabled = false
        }
        preferredSize = Display.dim_small
        listenTo(keys)
        // Restrict the possible keys entered to digits
        reactions += {
            case e: KeyTyped => if (!e.char.isDigit) {e.consume}     
        }
      }
      inc_textfields (i) = new TextField {
        font = Display.text_font
        try {
          // Try to draw the current value of the increment of this period
          text = Time.periods(i).inc.toString
        } catch {
          // If it is not defined yet, initializes the textfield with the default value
          case _ : Throwable => text = "0"
        }
        preferredSize = Display.dim_small
        listenTo(keys)
        // Restrict the possible keys entered to digits
        reactions += {
            case e: KeyTyped => if (!e.char.isDigit) {e.consume}     
        }
      }
    }

    // Once arrays has been filled, we define the BorderPanel
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

  /** Draw the sub-menu given in argument 
  *
  * @param id The id of the submenu we want to draw
  */
  class SubMenus (id : Int) extends GridPanel (1, 1) {
    id match {
      case 1 => contents += new DisplaySubMenu
      case 2 => contents += new PlayabilitySubMenu
      case 3 => contents += new ClockSubMenu (Time.nb_period)
    }
  }
}
