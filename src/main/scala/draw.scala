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
import sys.process._

/* This file is organised in objects, each of then draw a certain windows.
   To change the application window, we juste change the contents of Kasparov.frame in game.scala. */

/* The exception of the above rule : */

/**This class defines a grid of background cases,
   each background is juste a set of this grid. */

class BackgroundCase (i : Int, j : Int) extends GridPanel (i, j) {
	for(k <- 0 to i - 1) {
		for(l <- 0 to j - 1) {
			contents += new Label {
				/* The size of a grid (the same for the entire application) is difined in the object Constants
				   in game.scala, idem for paths to resources. */
				preferredSize = Constants.dim_small
				minimumSize = Constants.dim_small
				maximumSize = Constants.dim_small
				icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
			}
		}
	}
}

class PrettyBigLabel (text : String) extends Label (text) {
    border = new javax.swing.border.LineBorder (Color.black, 2)
	preferredSize = Constants.dim_message_drawer
	minimumSize = Constants.dim_message_drawer
	maximumSize = Constants.dim_message_drawer
	font = Constants.text_font
}

class PrettyBigButton extends Button {
	font = Constants.text_font
	preferredSize = Constants.dim_big
	minimumSize = Constants.dim_big
	maximumSize = Constants.dim_big
	border = new javax.swing.border.LineBorder (Color.black, 2)
}

/** Draw the main menu of the application.
	A menu is an instance of DrawMenu.Menu. */
object DrawMenu {

	/** This is the generic class for each option botton in the main menu,
	the role of a particular is defined depending on the parameter passed to the class. */
	class Option (name : String) extends PrettyBigButton {
		action = Action (name) {
			name match {
				case "<html><div style='text-align : center;'>Jouer une<br>partie classique</html>" =>
					Constants.alice_chess = false
					Ksparov.frame.contents = new DrawGameSelection.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "<html><div style='text-align : center;'>Jouer aux<br>échecs d'Alice</html>" =>
					Constants.alice_chess = true
					Ksparov.frame.contents = new DrawGameSelection.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Charger une partie" =>
                    DrawCharge.define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Voir les scores" => Ksparov.frame.contents = new DrawMenu.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Gérer les paramètres" => 
					Ksparov.frame.contents = new DrawParameters.SubMenus (1)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Quitter Ksparov" => Ksparov.frame.dispose()
			}
		}
	}

	/** The principal menu : for each line there are two buttons and a background between them. */
	class MenuGrid extends GridPanel (7, 3) {
    	for( i <- 0 to 6) {
    		i match {
	        	case 1 =>
	        		contents += new Option ("<html><div style='text-align : center;'>Jouer une<br>partie classique</html>")
					contents += new BackgroundCase (1, 3)
        			contents += new Option ("<html><div style='text-align : center;'>Jouer aux<br>échecs d'Alice</html>")
        		case 3 =>
        			contents += new Option ("Charger une partie")
        			contents += new BackgroundCase (1, 3)
        			contents += new Option ("Voir les scores")
        		case 5 =>
        			contents += new Option ("Gérer les paramètres")
        			contents += new BackgroundCase (1, 3)
        			contents += new Option ("Quitter Ksparov")
        		case _ =>
        			contents += new BackgroundCase (1, 3)
        			contents += new BackgroundCase (1, 3)
        			contents += new BackgroundCase (1, 3)
    		}
    	}
	}

	/** The final appearance with the principal menu between two columns of background cases. */
	class Menu extends BorderPanel {
    	layout (new BackgroundCase (7, 1)) = East
    	layout (new BackgroundCase (7, 1)) = West
    	layout (new MenuGrid) = Center
	}
}

/** This object is used to draw the menu for loading games. */
object DrawCharge {

	/** Returns the string passed in argument without the 4 last characters. */
    def shorten(s : String) : String = {
        return s.substring(Constants.save_path.length, s.length - 4)
    }

    def pred_nospace(s : String) : Boolean = {
      return !s.contains (' ')
    }

    var result : String = "";
    var listgame : List[String] = List("")
    var scroll = new ComboBox(listgame)
    var list_empty = false
    var text_label = ""

    /** Defines the value of result with the list of pgn files found in src/main/resources/Saves,
    and actualize other value depending on the fact that there is files or not. */
    def define_listgame {
        result = "find src/main/resources/Saves/ -regex .*[.]pgn " !!;
        if (result.length == 0) {
        	list_empty = true
        	text_label = "nosaves"
        } else {
        	list_empty = false
    		DrawCharge.listgame = result.split('\n').map(shorten).filter(pred_nospace).toList.sortBy(_.toLowerCase)
    		DrawCharge.scroll = new ComboBox(listgame) {
    	  	font = Constants.text_font
    	  }
        }
    }

    /** Defines the button for the loading menu. */
  	class Option (text : String, return_type : String) extends PrettyBigButton {
		action = Action (text) {
			return_type match {
				case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Game" =>
        	        Load.list_of_moves = List()
        	        Load.get_list_move_from_file(scroll.item)
        	        Constants.game_type = 6
        	        Ksparov.init_game(6)
        	        Ksparov.frame.contents = new DrawBoard.Board
					Ksparov.frame.peer.setLocationRelativeTo(null)
              case "Delete" =>
                    val res_ = ("rm " + Constants.save_path + scroll.item + ".pgn") !!;
                    define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
			}
		}
	}

	/** Defines the labels used in the loading menu, with the characteristics defined in Constants. */
	class PrettyLabel (text : String) extends PrettyBigLabel (text) {
		background = new Color (200, 200, 200)
		opaque = true
	}

	/** The main grid of the loading menu with everything. */
	class CenterGrid extends GridPanel (10,1) {
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
                    	contents += new Option  ("<html><div style='text-align : center;'>Supprimer la partie</html>", "Delete")
                    } else {
                    	contents += new BackgroundCase (1,5)
                    }
                case 6 =>
                    if (!list_empty) {
                    	contents += new Option ("<html><div style='text-align : center;'>Charger la partie</html>", "Game")
                    } else {
                    	contents += new PrettyLabel ("<html><div style='text-align : center;'> Chessgames.com pour télécharger des parties !</html>")
                    }
		  		case 8 => contents += new Option ("<html><div style='text-align : center;'>Revenir au menu</html>", "Menu")
		  		case _ => contents += new BackgroundCase (1, 5)
    		}
		}
	}

	/** The final menu with the central grid and background columns on each sides of it. */
	class Dcharge extends BorderPanel {
		define_listgame
    	layout (new BackgroundCase (10,1)) = East
    	layout (new BackgroundCase (10,1)) = West
    	layout (new CenterGrid) = Center
	}
}

object DrawSave {

	class SaveArgument (default : String, col : Int) extends TextField (default, col) {
		border = new javax.swing.border.LineBorder (Color.black, 2)
        preferredSize = Constants.dim_message_drawer
		minimumSize = Constants.dim_message_drawer
		maximumSize = Constants.dim_message_drawer
		font = Constants.text_font
		foreground = Color.black
		listenTo(mouse.clicks)
    	reactions += {
      		case _: event.MouseClicked =>
      			foreground = Color.black
      			text = ""
    	}
  	}

  	class SaveLabel (str : String) extends PrettyBigLabel (str) {
		preferredSize = Constants.dim_message_drawer
		minimumSize = Constants.dim_message_drawer
		maximumSize = Constants.dim_message_drawer
		background = new Color (200, 200, 200)
		opaque = true
  	}

    def resultgame (p : Int, gw : Boolean, gn : Boolean) : String = {
      (p, gw, gn) match {
        case (_, _, true) => "1/2-1/2"
        case (1, true, _) => "1-0"
        case (0, true, _) => "0-1"
        case _ => "*"
      }
    }

	val textFileName = new SaveArgument ("", 0)
	val textEvent = new SaveArgument ("Ksparov Tournament", 0)
	val textSite = new SaveArgument ("Ksparov Software", 0)
	val textDate = new SaveArgument (new SimpleDateFormat("yyyy.MM.dd").format(Calendar.getInstance().getTime()), 0)
	val textRound = new SaveArgument ("Ronde numéro ", 0)
	val textWhite = new SaveArgument ("Garry Kasparov", 0)
	val textBlack = new SaveArgument ("Bobby Fischer", 0)

	class ComeBack (text : String, return_type : String) extends PrettyBigButton {
		preferredSize = Constants.dim_message_drawer
		minimumSize = Constants.dim_message_drawer
		maximumSize = Constants.dim_message_drawer
		action = Action (text) {
			Save.write_to_file(textFileName.text.replaceAllLiterally(" ","_"), textEvent.text, textSite.text, textDate.text, textRound.text, textWhite.text, textBlack.text) match {
            	case 0 =>
					textFileName.text = ""
					return_type match {
						case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
							Ksparov.frame.peer.setLocationRelativeTo(null)
						case "Game" => Constants.timer = new TimeThread
    						Constants.ai_move = new AIMoveThread
    						Constants.thread_in_life = true
    						Ksparov.frame.contents = new DrawBoard.Board
	    					Constants.timer.start
    						Constants.ai_move.start
						case "Quit" => Ksparov.frame.dispose()
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

	class CancelButton extends PrettyBigButton {
		preferredSize = Constants.dim_message_drawer
		minimumSize = Constants.dim_message_drawer
		maximumSize = Constants.dim_message_drawer
		action = Action ("Annuler") {
			Ksparov.frame.contents = new DrawBoard.Board
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	class SwitchButton (switch_type : String) extends PrettyBigButton {
		preferredSize = Constants.dim_message_drawer
		minimumSize = Constants.dim_message_drawer
		maximumSize = Constants.dim_message_drawer
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

/* The window to choose and modify the parameters of the application : type of texture and type of pieces.
   Every parameter is recorded in the src/main/resources/Parameters file so they are kept from one gmae to the other.
   Other options will be added soon. */
object DrawParameters {

	class SubMenuChoice (id : Int, current_menu : Boolean) extends Button {
		preferredSize = new Dimension (70, 70)
		maximumSize = new Dimension (70, 70)
		minimumSize = new Dimension (70, 70)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + "parameter_sub_menu" + id + ".png")
			background = new Color (0, 0, 0)
			if (current_menu) {
				border = new javax.swing.border.LineBorder (Color.red, 2)
			}
			def apply = {
				Ksparov.frame.contents = new DrawParameters.SubMenus(id)
			}
		}
	}

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

	class ComeBack extends Button {
		font = Constants.text_font
		preferredSize = new Dimension (630, 70)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = Action("<html><div style='text-align : center;'>Appliquer les changements<br>et revenir au menu</html>") {
			Ksparov.frame.contents = new DrawMenu.Menu
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	/* Buttons for the texture choice, the parameter defines which texture the button stands for. */
	class TextureOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		/* The border is red for the actual parameter and black for other. The regural expression
		   get the number in the texture path. */
		if ((Constants.pattern findAllIn Constants.texture_path).mkString.toInt == number) {
			border = new javax.swing.border.LineBorder (Color.red, 2)
		} else {
			border = new javax.swing.border.LineBorder (Color.black, 2)
		}
		action = new Action("") {
			/* The action is different for other button because to have an icon on a button, you have to put in the action. */
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Texture_small_" + number.toString + ".png")
			def apply = {
				/* When the button is pushed, we write the new parameters in the src/main/resources/Parameters file.
				   After doing this, we apply the new parameters to that choice is dynamic. */
				Constants.write_parameters ((Constants.pattern findAllIn Constants.pieces_path).mkString, number.toString)
				Constants.apply_parameters
				Ksparov.frame.contents = new DrawParameters.SubMenus (1)
			}
		}
	}

	/* Button fot the piece type choice, excatly the same as before. */
	class PieceOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		if ((Constants.pattern findAllIn Constants.pieces_path).mkString.toInt == number) {
			border = new javax.swing.border.LineBorder (Color.red, 2)
		} else {
			border = new javax.swing.border.LineBorder (Color.black, 2)
		}
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Pieces/" + number.toString + "/1/King.png")
			def apply {
				Constants.write_parameters (number.toString, (Constants.pattern findAllIn Constants.texture_path).mkString (","))
				Constants.apply_parameters
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
		font = Constants.text_font
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
			contents += new ComeBack
			contents += new BackgroundCase (1, 9)
		}) = Center

		layout (new BackgroundCase (9, 1)) = East
	}

	class Increment (variable : String, sub_menu_id : Int) extends GridPanel (1, 3) {
		class IncButton (sign : String) extends Button {
			preferredSize = Constants.dim_small
			maximumSize = Constants.dim_small
			minimumSize = Constants.dim_small
			font = Constants.text_font
			action = Action (sign) {
				if (sign == "-") {
					variable match {
						case "ai_speed" => Constants.ai_speed = math.max (0, Constants.ai_speed - 100)
						case "nb_alice_board" => Constants.nb_alice_board = math.max (2, Constants.nb_alice_board - 1)
						case "nb_period" => Constants.nb_period = math.max (0, Constants.nb_period - 1)
					}
				} else {
					variable match {
						case "ai_speed" => Constants.ai_speed += 100
						case "nb_alice_board" => Constants.nb_alice_board += 1
						case "nb_period" => Constants.nb_period += 1
					}
				}
				Ksparov.frame.contents = new DrawParameters.SubMenus (sub_menu_id)
			}
		}

		contents += new IncButton ("-")
		contents += new Label {
			preferredSize = Constants.dim_small
			variable match {
				case "ai_speed" => text = Constants.ai_speed.toString
				case "nb_alice_board" => text = Constants.nb_alice_board.toString
				case "nb_period" => text = Constants.nb_period.toString
			}
		}
		contents += new IncButton ("+")
	}

	class SpeedAI extends BorderPanel {
		layout (new BackgroundCase (1, 2)) = West

		layout (new GridPanel (1, 2) {
			contents += new Label {
				preferredSize = Constants.dim_big
				font = Constants.text_font
				text = "<html><div style='text-align : center;'>Vitesse de jeu de l'IA<br>en milliseconde</html>"
			}
			contents += new Increment ("ai_speed", 2)
		}) = Center

		layout (new BackgroundCase (1, 1)) = East
	}

	class NbAliceBoard extends BorderPanel {
		layout (new BackgroundCase (1, 2)) = West

		layout (new GridPanel (1, 2) {
			contents += new Label {
				preferredSize = Constants.dim_big
				font = Constants.text_font
				text = "<html><div style='text-align : center;'>Nombre de plateau<br>de jeu d'Alice</html>"
			}
			contents += new Increment ("nb_alice_board", 2)
		}) = Center

		layout (new BackgroundCase (1, 1)) = East
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
			contents += new ComeBack
			contents += new BackgroundCase (1, 9)
		}) = Center

		layout (new BackgroundCase (7, 1)) = East	
	}

	class NbPeriodChoice extends BorderPanel {
		layout (new BackgroundCase (1, 2)) = West

		layout (new GridPanel (1, 2) {
			contents += new Label {
				preferredSize = Constants.dim_big
				font = Constants.text_font
				text = "Nombre de période"
			}
			contents += new Increment ("nb_period", 3)
		}) = Center

		layout (new BackgroundCase (1, 1)) = East
	}

	class PeriodOptions extends BorderPanel {
		layout (new BorderPanel {
			layout (new Label ("Temps") {preferredSize = new Dimension (140, 70)}) = West
			layout (new TextField () {preferredSize = new Dimension (70, 70)}) = East
		}) = West

		layout (new BorderPanel {
			layout (new Label ("Nombre de coup") {preferredSize = new Dimension (140, 70)}) = West
			layout (new TextField () {preferredSize = new Dimension (70, 70)}) = East
		}) = Center

		layout (new BorderPanel {
			layout (new Label ("Incrément") {preferredSize = new Dimension (140, 70)}) = West
			layout (new TextField () {preferredSize = new Dimension (70, 70)}) = East
		}) = East
	}

	class Periods (nb_period : Int) extends GridPanel (2 * nb_period + 1, 1) {
		for (i <- 0 to 2 * nb_period) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 9)
			} else {
				contents += new PeriodOptions
			}
		}
	}

	class ClockSubMenu (nb_period : Int) extends BorderPanel {
		layout (new BorderPanel {
			layout (new BackgroundCase (math.max (2 * nb_period + 5, 7), 1)) = West
			layout (new ChoiceColumn (math.max (2 * nb_period + 5, 7), 3)) = East
		}) = West

		layout (new BorderPanel {
			layout (new BorderPanel {
				layout (new BackgroundCase (1, 9)) = North
				layout (new NbPeriodChoice) = South
			}) = North
			if (Constants.nb_period == 0) {
				layout (new BackgroundCase (3, 9)) = Center
			} else {
				layout (new Periods (nb_period)) = Center
			}
			layout (new BorderPanel {
				layout (new ComeBack) = North
				layout (new BackgroundCase (1, 9)) = South
			}) = South
		}) = Center

		layout (new BackgroundCase (math.max (2 * nb_period + 5, 7), 1)) = East
	}

	/* The final menu with the texture choice first then the piece choice and finally a come back button. */
	class SubMenus (id : Int) extends GridPanel (1, 1) {
		id match {
			case 1 => contents += new DisplaySubMenu
			case 2 => contents += new PlayabilitySubMenu
			case 3 => contents += new ClockSubMenu (Constants.nb_period)
		}
	}

}

/* This object is for the game selection : Human vs Human, AI vs AI or AI vs Human. */
object DrawGameSelection {

	class MessageDrawer extends Label {
		font = Constants.text_font
		background = new Color (200, 200, 200)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		opaque = true
		if (Constants.alice_chess) {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs d'Alice,<br>veuillez sélectionner le type de jeu !</html>"
		} else {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs classiques,<br>veuillez sélectionner le type de jeu !</html>"
		}
	}

	/* The button for the choice selection : change the value of Constants.game_type when pressed. */
	class Option (name : String, num : Int) extends PrettyBigButton {
		action = Action (name) {
			Constants.game_type = num
			/* Launched a game. */
		    Ksparov.init_game (num)
    		Ksparov.frame.contents = new DrawBoard.Board
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	/* The menu with options and a come back button. */
	class CenterGrid extends GridPanel (7, 3) {
		for (i <- 0 to 6) {
			i match {
				case 1 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>couleur aléatoire</html>", 2)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("Humain vs Humain", 1)
				case 3 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>jouer les blancs</html>", 3)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("IA vs IA", 5)
				case 5 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>jouer les noirs</html>", 4)
					contents += new BackgroundCase (1, 3)
					contents += new Button {
						font = Constants.text_font
						border = new javax.swing.border.LineBorder (Color.black, 2)
						action = Action ("Revenir au menu") {
							Ksparov.frame.contents = new DrawMenu.Menu
							Ksparov.frame.peer.setLocationRelativeTo(null)
						}
					}
				case _ =>
					contents += new BackgroundCase (1, 3)
					contents += new BackgroundCase (1, 3)
					contents += new BackgroundCase (1, 3)
			}
		}
	}

	class WelcomeMessage extends BorderPanel {
		layout (new BackgroundCase (1, 11)) = North
		layout (new BorderPanel {
			layout (new BackgroundCase (1, 1)) = West
			layout (new MessageDrawer) = Center
			layout (new BackgroundCase (1, 1)) = East
		}) = South

	}

	/* The final class with background. */
	class Menu extends BorderPanel {
		layout (new BackgroundCase (7, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (7, 1)) = West
		layout (new WelcomeMessage) = North
	}
}

/* The most important class : draw the board itself ! */
object DrawBoard {

	class Clock (player : Int) extends Label {
		foreground = new Color ((1 - player) * 255, (1 - player) * 255, (1 - player) * 255)
		background = new Color (player * 255, player * 255, player * 255)
		opaque = true
	    border = new javax.swing.border.LineBorder (Color.black, 2)
		preferredSize = Constants.dim_big
		minimumSize = Constants.dim_big
		maximumSize = Constants.dim_big
		font = Constants.text_font
		if (Constants.players(player).actual_period + 1 == Constants.periods.length) {
			text = "<html><div style='text-align : center;'>" + Time.int_to_hhmmss(Constants.players(player).actual_time) + "<br>" + "Dernière période</html>"
		} else {
			text = "<html><div style='text-align : center;'>" + Time.int_to_hhmmss(Constants.players(player).actual_time) + "<br>" + "Encore " + (math.max(Constants.periods(Constants.players(player).actual_period).nb_move - Constants.players(player).nb_move, 0)) + " coups </html>"
		}
	}

	/* We need here background cases with label in order to have letters and numbers around the board. */
	class BackgroundCaseWithLabel (label : String) extends Label (label) {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
		/* This option make the superposition of text and icon possible. */
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
	}

	/* These cases just have an icon a piece (except king), they are on the left and right side of the bord
	    and they are used to count the number of each dead piece for each player. */
	class DeadCase (player : Int, piece : String) extends Button {
		preferredSize = Constants.dim_small
		/* They are colored in a grey close to the lead color. */
		background = new Color (121, 128, 129)
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player.toString + "/" + piece + ".png")
			disabledIcon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player.toString + "/" + piece + ".png")
			border = new javax.swing.border.LineBorder (Color.black, 1)
			enabled = false
			def apply = {
				Constants.selected_promotion = piece
				Ksparov.promotion (Constants.curr_player)
			}
		}
	}

	/* These cases are the just next to the previous one, they are numbers of dead pieces. */
	class NumDeadCase (player : Int, number : Int) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
		font = Constants.num_dead_font
		text = number.toString
	}

	/* The case of the board, definend by the coordinates. They are alternatively black and white
	   depending on their position. They go from (0, 0) to (7, 7). */
	class Case (x : Int, y : Int, grid_id : Int) extends Button {
		preferredSize = Constants.dim_small
		if ((x + y) % 2 == 0) {
			background = Color.black
		} else {
			background = Color.white
  		}
		action = new Action ("") {
			/*icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + "1/Queen.png")*/
			def apply = {
				/* When we click one a case, Constants.selected_case receive it in a one dimension way.
				   Then we launched the movment method in game.scala. */
				Constants.selected_case = x + y * 8
				Constants.selected_grid = grid_id
                Ksparov.play_move
			}
		}
	}

	/* The board with its 63 cases, it is represented as an Array in Constants.grid_cases,
	   we need it in an array to change the icon variable depending on board.
	   The dimension is now in one dimension because mutli dimension array in scala are not well supported. */
	def init_grids {
		if (Constants.alice_chess) {
			Constants.nb_grid = 2
		} else {
			Constants.nb_grid = 1
		}
		Constants.grids = new Array [Array[DrawBoard.Case]] (Constants.nb_grid)
		for(i <- 0 to Constants.nb_grid - 1) {
			Constants.grids (i) = new Array [Case] (Constants.nb_case_board * Constants.nb_case_board)
		}
	}

	def create_grid_cases {
		for (k <- 0 to Constants.nb_grid - 1) {
			for (i <- 0 to Constants.nb_case_board - 1) {
				for (j <- 0 to Constants.nb_case_board - 1) {
					Constants.grids (k) (i + j * 8) = new Case (i, j, k)
				}
			}
		}
	}

	def create_grid_dead {
		for (j <- 0 to 1) {
			Constants.promotion_buttons(j)(0) = new DeadCase (j, "Queen")
			Constants.promotion_buttons(j)(1) = new DeadCase (j, "Bishop")
			Constants.promotion_buttons(j)(2) = new DeadCase (j, "Knight")
			Constants.promotion_buttons(j)(3) = new DeadCase (j, "Rook")
		}
	}

	/* The center grid with the board and background with label around. */
	class Simple_Grid (grid_id : Int) extends GridPanel (Constants.nb_case_board + 2, Constants.nb_case_board + 1) {
		for (i <- -1 to Constants.nb_case_board) {
			if (i == - 1 || i == Constants.nb_case_board) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCaseWithLabel ("A")
				contents += new BackgroundCaseWithLabel ("B")
				contents += new BackgroundCaseWithLabel ("C")
				contents += new BackgroundCaseWithLabel ("D")
				contents += new BackgroundCaseWithLabel ("E")
				contents += new BackgroundCaseWithLabel ("F")
				contents += new BackgroundCaseWithLabel ("G")
				contents += new BackgroundCaseWithLabel ("H")
			} else {
				for (j <- -1 to Constants.nb_case_board - 1) {
					if (j == -1) {
						contents += new BackgroundCaseWithLabel ((8 - i).toString)
					} else {
						/* Using 7 - i here because we want "classic" axis from left to right and from bottom to top. */
						contents += Constants.grids (grid_id) (j + (7 - i) * 8)
					}
				}
			}
		}
	}

	class Number_column extends GridPanel (Constants.nb_case_board + 2, 1) {
		for (i <- 0 to Constants.nb_case_board + 1) {
			if (i < 1 || i > 8) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new BackgroundCaseWithLabel ((9 - i).toString)
			}
		}
	}

	class Grid extends GridPanel (1, Constants.nb_grid) {
		for (k <- 0 to Constants.nb_grid - 1) {
			contents += new Simple_Grid (k)
		}
	}

	/* The board has a menu on his top defined here. */
	class Header extends GridPanel (2, 2) {
		contents += new Button {
		font = Constants.text_font
			action = Action ("Recommencer une partie") {
				Constants.thread_in_life = false
	    		Ksparov.frame.contents = new DrawGameSelection.Menu
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Sauvegarder la partie") {
				Constants.thread_in_life = false
				Ksparov.frame.contents = new DrawSave.SimpleSave
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Revenir au menu principal") {
				Constants.thread_in_life = false
				Ksparov.frame.contents = new DrawMenu.Menu
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Quitter Ksparov") {
				Constants.thread_in_life = false
	    		Ksparov.frame.dispose()
			}
		}
	}

	/* Behind the board, there is the message drawer, which is a label where messages like "check", "mat"... are displayed. */
	class MessageDrawer (message : String) extends Label (message) {
		font = Constants.text_font
		preferredSize = Constants.dim_message_drawer
		background = new Color (220, 220, 220)
		opaque = true
		foreground = Color.black
		horizontalTextPosition = Alignment.Center
		verticalTextPosition = Alignment.Center
		border = new javax.swing.border.LineBorder (Color.black, 1)
	}

	/* The footer of the window with the message drawer. */
	class Footer extends BorderPanel {
		layout(new BackgroundCase (1, 2)) = East
		layout(new BackgroundCase (1, 2)) = West
		layout(new BorderPanel {
			layout (if (Constants.game_type == 6) {
					new BackgroundCase (1, 3)
				} else {
					new Clock (1)
				}) = West
			layout (Constants.message_drawer) = Center
			layout (if (Constants.game_type == 6) {
					new BackgroundCase (1, 3)
				} else {
					new Clock (0)
				}) = East
		}) = Center
	}

	class PlayButton (player : Int) extends Button {
		preferredSize = Constants.dim_small
		action = new Action ("") {
			enabled = false
			borderPainted = false
			icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
			disabledIcon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
			if (Constants.game_type == 6) {
				enabled = true
				background = Color.red
				borderPainted = true
				border = new javax.swing.border.LineBorder (Color.black, 1)
				icon = new javax.swing.ImageIcon(Constants.resources_path + "Play_button" + player.toString + ".png")
			}
			def apply {
				Constants.players (player) = new Human (player)
				Constants.players (1 - player) = new AI (1 - player)
				for (k <- 0 to 1) {
					Constants.play_buttons(k).enabled = false
					Constants.play_buttons(k).borderPainted = false
					Constants.play_buttons(k).icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.texture_path)
				}
			}
		}
	}

	/* Border grid with dead pieces for the white player. */
	class Border1 extends GridPanel (Constants.nb_case_board + 2, 3) {
		for(i <- 0 to Constants.nb_case_board + 1) {
			i match {
				case 3 =>
					contents += Constants.play_buttons (1)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
				case 4 =>
					contents += Constants.promotion_buttons(1)(0)
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(0))
					contents += new BackgroundCase (1, 1)
				case 5 =>
					contents += Constants.promotion_buttons(1)(1)
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(1))
					contents += new BackgroundCase (1, 1)
				case 6 =>
					contents += Constants.promotion_buttons(1)(2)
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(2))
					contents += new BackgroundCase (1, 1)
				case 7 =>
					contents += Constants.promotion_buttons(1)(3)
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(3))
					contents += new BackgroundCase (1, 1)
				case 8 =>
					contents += new DeadCase (1, "Pawn")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(4))
					contents += new BackgroundCase (1, 1)
				case _ =>
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
			}
		}
	}

	/* Border grid with dead pieces for the black player. */
	class Border0 extends GridPanel (Constants.nb_case_board + 2, 4) {
		for(i <- 0 to Constants.nb_case_board + 1) {
			if (i < 1 || i > 8) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else {
				if (i > 6) {
					contents += new BackgroundCaseWithLabel ((9 - i).toString)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
				} else { i match {
					case 1 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Constants.dead_pieces(0)(0))
						contents += Constants.promotion_buttons(0)(0)
					case 2 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Constants.dead_pieces(0)(1))
						contents += Constants.promotion_buttons(0)(1)
					case 3 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Constants.dead_pieces(0)(2))
						contents += Constants.promotion_buttons(0)(2)
					case 4 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Constants.dead_pieces(0)(3))
						contents += Constants.promotion_buttons(0)(3)
					case 5 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Constants.dead_pieces(0)(4))
						contents += new DeadCase (0, "Pawn")
					case 6 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new BackgroundCase (1, 1)
						contents += Constants.play_buttons (0)
					}
				}
			}
		}
	}

	/* The final board with everything. */
	class Board extends BorderPanel {
		layout(new Header) = North
		layout(new Border1) = West
		layout(new Border0) = East
		layout(new Footer) = South
		layout(new Grid) = Center
	}
}

/* Special object here which contains every methods for drawing actions to the board. */
object DrawActions {

	/* Draw a game board (an array of pieces) on the chessboard. */
	def draw_game_board (game_board : Array[Piece]) {
		/* Reinitializing the dead piece array to avoid mutli-counting. */
		Constants.dead_pieces = Array(new Array[Int](5), new Array[Int](5))
		/* Initilizing the array of cases. */
		DrawBoard.create_grid_cases
		/* For each piece in the game board. */
		for (i <- 0 to game_board.length - 1) {
			/* Transcripting coordinates in one dimension. */
			var coord = game_board(i).pos_x + game_board(i).pos_y * 8
			/* If the piece is alive, update the icon of the case of its position. */
			if (coord >= 0) {
				var piece_path = game_board(i).piece_path
				Constants.grids(game_board(i).grid)(coord).action.icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + game_board(i).player.toString + "/" + piece_path)
			/* Else, if the piece is dead, update the array which counts the number of dead piece for each players. */
			} else {
				game_board(i).name match {
					case "queen" => Constants.dead_pieces(game_board(i).player)(0) += 1
					case "bishop" => Constants.dead_pieces(game_board(i).player)(1) += 1
					case "knight" => Constants.dead_pieces(game_board(i).player)(2) += 1
					case "rook" => Constants.dead_pieces(game_board(i).player)(3) += 1
					case "pawn" => Constants.dead_pieces(game_board(i).player)(4) += 1
				}
			}
		}
		/* Draw the new board. */
	}

	def enable_promotion (p : Int) {
		Constants.promotion = true
		for (i <- 0 to 3) {
			Constants.promotion_buttons(p)(i).enabled = true
			Constants.promotion_buttons(p)(i).background = Color.red
		}
		Ksparov.frame.contents = new DrawBoard.Board
	}

	def disable_promotion (p : Int) {
		Constants.promotion = false
		for (i <- 0 to 3) {
			Constants.promotion_buttons(p)(i).enabled = false
			Constants.promotion_buttons(p)(i).background = new Color (121, 128, 129)
		}
		draw_game_board (Ksparov.board)
		Ksparov.frame.contents = new DrawBoard.Board
		draw_game_messages ("Current_turn", 1 - Constants.curr_player)
	}

	/* Color in red the reachables cases. */
	def draw_possible_moves (case_list : Array[(Int, Int)], pice_position_x : Int, piece_position_y : Int, grid : Int) {
		/* Coloring the selected case in red. */
		Constants.grids(grid)(pice_position_x + piece_position_y * 8).background = Color.yellow
		Constants.grids((grid + 1) % (Constants.nb_grid))(pice_position_x + piece_position_y * 8).background = Color.yellow

		/* For each reachable case, colors it into red. */
   		for (i <- 0 to case_list.length - 1) {
            var (j, l) = case_list(i)
            Constants.grids (grid) (j + 8 * l).background = Color.red
            Constants.grids ((grid + 1) % (Constants.nb_grid)) (j + 8 * l).background = Color.red
        }
	}

	/* Recolor as "normal" cases : white and black. */
	def clear_possible_moves {
		for (k <- 0 to Constants.nb_grid - 1) {
			for (i <- 0 to 63) {
				if ((i % 8 + i / 8) % 2 == 0) {
					Constants.grids(k)(i).background = Color.black
				} else {
					Constants.grids(k)(i).background = Color.white
				}
			}
		}
	}

	/* Draw a given message on the board, the message depends on the argument passed */
	def draw_game_messages (message_type : String, player : Int) {
        var joueur_string = Constants.game_type match {
        	case 6 =>
            	player match {
                	case 1 => Load.infos("White") + " (BLANC) "
                	case 0 => Load.infos("Black") + " (NOIR) "
            	}
            case _ =>
            	player match {
               		case 1 => "joueur blanc"
                	case 0 => "joueur noir"
            	}
          }

		message_type match {

			/* Draw who's player the turn is. */
			case "Current_turn" => Constants.game_type match {
				case 6 => Constants.message_drawer.text = "La main est à " + joueur_string +" !"
					Constants.message_drawer.foreground = Color.black
				case _ => Constants.message_drawer.text = "La main est au " + joueur_string +" !"
					Constants.message_drawer.foreground = Color.black
			}

			/* Draw if a player is in check. */
			case "Check" => Constants.game_type match {
				case 6 => Constants.message_drawer.text = joueur_string + " est en échec !"
					Constants.message_drawer.foreground = Color.red
				case _ => Constants.message_drawer.text = "Le " + joueur_string + " est en échec !"
					Constants.message_drawer.foreground = Color.red
			}

			case "Time" => Constants.message_drawer.text = "<html><div style='text-align : center;'>Perte au temps,<br>" + joueur_string + " gagne la partie !</html>"
				Constants.message_drawer.foreground = Color.red

			/* Draw if a player is mate. */
			case "Mate" => Constants.game_type match {
				case 6 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>" + (if (player == 0) {Load.infos("White")} else {Load.infos("Black")}) + " gagne la partie !</html>"
					Constants.message_drawer.foreground = Color.red
				case _ => Constants.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>le "+ (if(player == 0){"joueur blanc"}else{"joueur noir"}) + " gagne la partie !</html>"
					Constants.message_drawer.foreground = Color.red
			}

			/* Draw if an AI cannot move (this option has only been implemented for IA). */
			case "Pat" => Constants.game_type match {
				case 6 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>"+ joueur_string +" ne peut plus bouger !</html>"
					Constants.message_drawer.foreground = Color.red
				case _ => Constants.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>le "+ joueur_string +" ne peut plus bouger !</html>"
					Constants.message_drawer.foreground = Color.red
			}
			/* Draw if there is no more checkmate possible */
			case "Nulle" => Constants.game_type match {
				case 6 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Partie nulle : "+ joueur_string +" ne peut plus mater !</html>"
					Constants.message_drawer.foreground = Color.red
				case _ => Constants.message_drawer.text = "<html><div style='text-align : center;'>Partie nulle : le "+ joueur_string +" ne peut plus mater !</html>"
					Constants.message_drawer.foreground = Color.red
			}
			/* Draw if 50 moves has been made without a pawn move or a piece taken */
			case "50coups" => Constants.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 50 coups sans prise ni mouvement de pion ! </html>"
				Constants.message_drawer.foreground = Color.red
			/*Draw if a single position occured 3 times in the same game */
			case "TripleRepetition" => 	Constants.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 3 répétitions de la même position ! </html>"
				Constants.message_drawer.foreground = Color.red

			case "Promotion" => Constants.curr_player match {
				case 0 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion noir !"
				case 1 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion blanc !"
			}

        	case "1-0" => Constants.message_drawer.text = "<html><div style='text-align : center;'>" + Load.infos("White") +" gagne la partie !</html>"
				Constants.message_drawer.foreground = Color.red

        	case "0-1" => Constants.message_drawer.text = "<html><div style='text-align : center;'>"+ Load.infos("Black") +" gagne la partie !</html>"
				Constants.message_drawer.foreground = Color.red

        	case "1/2-1/2" => Constants.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle !</html>"
				Constants.message_drawer.foreground = Color.red

        	case "*" => Constants.message_drawer.text = "<html><div style='text-align : center;'>Partie non finie !</html>"
				Constants.message_drawer.foreground = Color.red

        	case "!$" => Constants.message_drawer.text = "Bon coup de " + joueur_string +" !"
				Constants.message_drawer.foreground = Color.black
        	case "!!" => Constants.message_drawer.text = "Trés bon coup de " + joueur_string +" !"
				Constants.message_drawer.foreground = Color.black
        	case "?$" =>Constants.message_drawer.text = "Que fait " + joueur_string +" ?"
				Constants.message_drawer.foreground = Color.black
        	case "??" => Constants.message_drawer.text = "Coup trés surprenant de la part de " + joueur_string +" !"
				Constants.message_drawer.foreground = Color.black
        	case "!?" => Constants.message_drawer.text =  joueur_string +" nous cache-t-il quelque chose ?"
				Constants.message_drawer.foreground = Color.black
        	case "?!" => Constants.message_drawer.text = "Qu'espère  " + joueur_string + " en jouant ce coup ?"
				Constants.message_drawer.foreground = Color.black
		}
	}
}
