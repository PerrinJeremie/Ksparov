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
	for (k <- 0 to i - 1) {
		for(l <- 0 to j - 1) {
			contents += new Label {
				/* The size of a grid (the same for the entire application) is difined in the object Display
				   in game.scala, idem for paths to resources. */
				preferredSize = Display.dim_small
				minimumSize = Display.dim_small
				maximumSize = Display.dim_small
				icon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
			}
		}
	}
}

/** The label we will use with preferences already set */
class PrettyBigLabel (text : String) extends Label (text) {
    border = new javax.swing.border.LineBorder (Color.black, 2)
	preferredSize = Display.dim_message_drawer
	minimumSize = Display.dim_message_drawer
	maximumSize = Display.dim_message_drawer
	font = Display.text_font
}

/** The button we will use with preferences already set */
class PrettyBigButton extends Button {
	font = Display.text_font
	preferredSize = Display.dim_big
	minimumSize = Display.dim_big
	maximumSize = Display.dim_big
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
					Ksparov.frame.contents = new DrawGameSelection.Menu (false)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "<html><div style='text-align : center;'>Jouer aux<br>échecs d'Alice</html>" =>
					Ksparov.frame.contents = new DrawGameSelection.Menu (true)
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
        return s.substring(Display.save_path.length, s.length - 4)
    }

    /** Predicates if a string contains at least one space character. */
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

    /** Defines the button for the loading menu. */
  	class Option (text : String, return_type : String) extends PrettyBigButton {
		action = Action (text) {
			return_type match {
				case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Game" =>
        	        Ksparov.curr_game = new Ksparov.Game (6, 1, false)
        	        Load.list_of_moves = List()
        	        Load.get_list_move_from_file(scroll.item)
        	        Ksparov.init_game(6)
        	        Ksparov.frame.contents = new DrawBoard.Board
					Ksparov.frame.peer.setLocationRelativeTo(null)
              case "Delete" =>
                    val res_ = ("rm " + Display.save_path + scroll.item + ".pgn") !!;
                    define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
			}
		}
	}

	/** Defines the labels used in the loading menu, with the characteristics defined in Display. */
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

/** Draws the menus used to save games.*/
object DrawSave {

    /** A text field that stores a certain information conserning the save, to be filled by the user. */
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

    /** A label. */
  	class SaveLabel (str : String) extends PrettyBigLabel (str) {
		preferredSize = Display.dim_message_drawer
		minimumSize = Display.dim_message_drawer
		maximumSize = Display.dim_message_drawer
		background = new Color (200, 200, 200)
		opaque = true
  	}

    /** Returns the result tag associated with (player, game_won, game_null). */
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

    /** Button component which saves game and brings you back : to the menu with "Menu" as argument, to the game with "Game" as argument, to quit Ksparov with "Quit" as argument.*/
	class ComeBack (text : String, return_type : String) extends PrettyBigButton {
		preferredSize = Display.dim_message_drawer
		minimumSize = Display.dim_message_drawer
		maximumSize = Display.dim_message_drawer
		action = Action (text) {
			Save.write_to_file(textFileName.text.replaceAllLiterally(" ","_"), textEvent.text, textSite.text, textDate.text, textRound.text, textWhite.text, textBlack.text) match {
            	case 0 =>
					textFileName.text = ""
					return_type match {
						case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
							Ksparov.frame.peer.setLocationRelativeTo(null)
						case "Game" => Ksparov.curr_game.timer = new Time.TimeThread
    						Ksparov.curr_game.ai_move = new AIMoveThread
    						Ksparov.curr_game.thread_in_life = true
    						Ksparov.frame.contents = new DrawBoard.Board
	    					Ksparov.curr_game.timer.start
    						Ksparov.curr_game.ai_move.start
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

    /** Button switches between simple-save (only choice is filename), and advanced-save (all choices). */
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

    /** A panel which regroups all components of the advanced-menu */
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

/* This object is for the game selection : Human vs Human, AI vs AI or AI vs Human. */
object DrawGameSelection {

	class MessageDrawer (alice : Boolean) extends Label {
		font = Display.text_font
		background = new Color (200, 200, 200)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		opaque = true
		if (alice) {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs d'Alice,<br>veuillez sélectionner le type de jeu !</html>"
		} else {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs classiques,<br>veuillez sélectionner le type de jeu !</html>"
		}
	}

	/* The button for the choice selection : change the value of Ksparov.curr_game.game_type when pressed. */
	class Option (name : String, num : Int, alice : Boolean) extends PrettyBigButton {
		action = Action (name) {
			if (alice) {
				Ksparov.curr_game = new Ksparov.Game (num, Parameters.nb_alice_board, alice)

			} else {
				Ksparov.curr_game = new Ksparov.Game (num, 1, alice)
			}
			/* Launched a game. */
		    Ksparov.init_game (num)
    		Ksparov.frame.contents = new DrawBoard.Board
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	/* The menu with options and a come back button. */
	class CenterGrid (alice : Boolean) extends GridPanel (7, 3) {
		for (i <- 0 to 6) {
			i match {
				case 1 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>couleur aléatoire</html>", 2, alice)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("Humain vs Humain", 1, alice)
				case 3 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>jouer les blancs</html>", 3, alice)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("IA vs IA", 5, alice)
				case 5 =>
					contents += new Option ("<html><div style='text-align : center;'>Humain vs IA<br>jouer les noirs</html>", 4, alice)
					contents += new BackgroundCase (1, 3)
					contents += new Button {
						font = Display.text_font
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

	class WelcomeMessage (alice : Boolean) extends BorderPanel {
		layout (new BackgroundCase (1, 11)) = North
		layout (new BorderPanel {
			layout (new BackgroundCase (1, 1)) = West
			layout (new MessageDrawer (alice)) = Center
			layout (new BackgroundCase (1, 1)) = East
		}) = South

	}

	/* The final class with background. */
	class Menu (alice : Boolean) extends BorderPanel {
		layout (new BackgroundCase (7, 1)) = East
		layout (new CenterGrid (alice)) = Center
		layout (new BackgroundCase (7, 1)) = West
		layout (new WelcomeMessage (alice)) = North
	}
}

/* The most important class : draw the board itself ! */
object DrawBoard {

	class Clock (player : Int) extends Label {
		foreground = new Color ((1 - player) * 255, (1 - player) * 255, (1 - player) * 255)
		background = new Color (player * 255, player * 255, player * 255)
		opaque = true
	    border = new javax.swing.border.LineBorder (Color.black, 2)
		preferredSize = Display.dim_big
		minimumSize = Display.dim_big
		maximumSize = Display.dim_big
		font = Display.text_font
		if (Ksparov.curr_game.players(player).actual_period + 1 == Time.periods.length) {
			text = "<html><div style='text-align : center;'>" + Time.int_to_hhmmss(Ksparov.curr_game.players(player).actual_time) + "<br>" + "Dernière période</html>"
		} else {
			text = "<html><div style='text-align : center;'>" + Time.int_to_hhmmss(Ksparov.curr_game.players(player).actual_time) + "<br>" + "Encore " + (math.max(Time.periods(Ksparov.curr_game.players(player).actual_period).nb_move - Ksparov.curr_game.players(player).nb_move, 0)) + " coups </html>"
		}
	}

	/* We need here background cases with label in order to have letters and numbers around the board. */
	class BackgroundCaseWithLabel (label : String) extends Label (label) {
		preferredSize = Display.dim_small
		icon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
		/* This option make the superposition of text and icon possible. */
		horizontalTextPosition = Alignment.Center
		foreground = Display.text_color
	}

	/* These cases just have an icon a piece (except king), they are on the left and right side of the bord
	    and they are used to count the number of each dead piece for each player. */
	class DeadCase (player : Int, piece : String) extends Button {
		preferredSize = Display.dim_small
		/* They are colored in a grey close to the lead color. */
		background = new Color (121, 128, 129)
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + player.toString + "/" + piece + ".png")
			disabledIcon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + player.toString + "/" + piece + ".png")
			border = new javax.swing.border.LineBorder (Color.black, 1)
			enabled = false
			def apply = {
				Ksparov.curr_game.selected_promotion = piece
				Ksparov.promotion (Ksparov.curr_game.curr_player)
			}
		}
	}

	/* These cases are the just next to the previous one, they are numbers of dead pieces. */
	class NumDeadCase (player : Int, number : Int) extends Label {
		preferredSize = Display.dim_small
		icon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
		horizontalTextPosition = Alignment.Center
		foreground = Display.text_color
		font = Display.num_dead_font
		text = number.toString
	}

	/* The case of the board, definend by the coordinates. They are alternatively black and white
	   depending on their position. They go from (0, 0) to (7, 7). */
	class Case (x : Int, y : Int, grid_id : Int) extends Button {
		preferredSize = Display.dim_small
		if ((x + y) % 2 == 0) {
			background = Color.black
		} else {
			background = Color.white
  		}
		action = new Action ("") {
			def apply = {
				/* When we click one a case, Ksparov.curr_game.selected_case receive it in a one dimension way.
				   Then we launched the movment method in game.scala. */
				Ksparov.curr_game.selected_case = x + y * 8
				Ksparov.curr_game.selected_grid = grid_id
                Ksparov.play_move
			}
		}
	}

	/* The board with its 63 cases, it is represented as an Array in Ksparov.curr_game.grid_cases,
	   we need it in an array to change the icon variable depending on board.
	   The dimension is now in one dimension because mutli dimension array in scala are not well supported. */
	def init_grids {
		Ksparov.curr_game.grids = new Array [Array[DrawBoard.Case]] (Ksparov.curr_game.nb_grid)
		for(i <- 0 to Ksparov.curr_game.nb_grid - 1) {
			Ksparov.curr_game.grids (i) = new Array [Case] (Parameters.nb_case_board * Parameters.nb_case_board)
		}
	}

	def create_grid_cases {
		for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
			for (i <- 0 to Parameters.nb_case_board - 1) {
				for (j <- 0 to Parameters.nb_case_board - 1) {
					Ksparov.curr_game.grids (k) (i + j * 8) = new Case (i, j, k)
				}
			}
		}
	}

	def create_grid_dead {
		for (j <- 0 to 1) {
			Ksparov.curr_game.promotion_buttons(j)(0) = new DeadCase (j, "Queen")
			Ksparov.curr_game.promotion_buttons(j)(1) = new DeadCase (j, "Bishop")
			Ksparov.curr_game.promotion_buttons(j)(2) = new DeadCase (j, "Knight")
			Ksparov.curr_game.promotion_buttons(j)(3) = new DeadCase (j, "Rook")
		}
	}

	/* The center grid with the board and background with label around. */
	class Simple_Grid (grid_id : Int) extends GridPanel (Parameters.nb_case_board + 2, Parameters.nb_case_board + 1) {
		for (i <- -1 to Parameters.nb_case_board) {
			if (i == - 1 || i == Parameters.nb_case_board) {
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
				for (j <- -1 to Parameters.nb_case_board - 1) {
					if (j == -1) {
						contents += new BackgroundCaseWithLabel ((8 - i).toString)
					} else {
						/* Using 7 - i here because we want "classic" axis from left to right and from bottom to top. */
						contents += Ksparov.curr_game.grids (grid_id) (j + (7 - i) * 8)
					}
				}
			}
		}
	}

	class Number_column extends GridPanel (Parameters.nb_case_board + 2, 1) {
		for (i <- 0 to Parameters.nb_case_board + 1) {
			if (i < 1 || i > 8) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new BackgroundCaseWithLabel ((9 - i).toString)
			}
		}
	}

	class Grid extends GridPanel (1, Ksparov.curr_game.nb_grid) {
		for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
			contents += new Simple_Grid (k)
		}
	}

	/* The board has a menu on his top defined here. */
	class Header extends GridPanel (2, 2) {
		contents += new Button {
		font = Display.text_font
			action = Action ("Recommencer une partie") {
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
	    		Ksparov.frame.contents = new DrawGameSelection.Menu (Ksparov.curr_game.alice_chess)
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Display.text_font
			action = Action ("Sauvegarder la partie") {
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				Ksparov.frame.contents = new DrawSave.SimpleSave
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Display.text_font
			action = Action ("Revenir au menu principal") {
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				Ksparov.frame.contents = new DrawMenu.Menu
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		contents += new Button {
		font = Display.text_font
			action = Action ("Quitter Ksparov") {
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
	    		Ksparov.frame.dispose()
			}
		}
	}

	/* Behind the board, there is the message drawer, which is a label where messages like "check", "mat"... are displayed. */
	class MessageDrawer (message : String) extends Label (message) {
		font = Display.text_font
		preferredSize = Display.dim_message_drawer
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
			layout (if (Time.clock_available) {
					new Clock (1)
				} else {
					new BackgroundCase (1, 3)
				}) = West
			layout (Ksparov.curr_game.message_drawer) = Center
			layout (if (Time.clock_available) {
					new Clock (0)
				} else {
					new BackgroundCase (1, 3)
				}) = East
		}) = Center
	}

	class PlayButton (player : Int) extends Button {
		preferredSize = Display.dim_small
		action = new Action ("") {
			enabled = false
			borderPainted = false
			icon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
			disabledIcon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
			if (Ksparov.curr_game.game_type == 6) {
				enabled = true
				background = Color.red
				borderPainted = true
				border = new javax.swing.border.LineBorder (Color.black, 1)
				icon = new javax.swing.ImageIcon(Display.resources_path + "Play_button" + player.toString + ".png")
			}
			def apply {
				Ksparov.curr_game.players (player) = new Human (player)
				Ksparov.curr_game.players (1 - player) = new AI (1 - player)
				for (k <- 0 to 1) {
					Ksparov.curr_game.play_buttons(k).enabled = false
					Ksparov.curr_game.play_buttons(k).borderPainted = false
					Ksparov.curr_game.play_buttons(k).icon = new javax.swing.ImageIcon(Display.resources_path + Display.texture_path)
				}
			}
		}
	}

	/* Border grid with dead pieces for the white player. */
	class Border1 extends GridPanel (Parameters.nb_case_board + 2, 3) {
		for(i <- 0 to Parameters.nb_case_board + 1) {
			i match {
				case 3 =>
					contents += Ksparov.curr_game.play_buttons (1)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
				case 4 =>
					contents += Ksparov.curr_game.promotion_buttons(1)(0)
					contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(1)(0))
					contents += new BackgroundCase (1, 1)
				case 5 =>
					contents += Ksparov.curr_game.promotion_buttons(1)(1)
					contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(1)(1))
					contents += new BackgroundCase (1, 1)
				case 6 =>
					contents += Ksparov.curr_game.promotion_buttons(1)(2)
					contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(1)(2))
					contents += new BackgroundCase (1, 1)
				case 7 =>
					contents += Ksparov.curr_game.promotion_buttons(1)(3)
					contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(1)(3))
					contents += new BackgroundCase (1, 1)
				case 8 =>
					contents += new DeadCase (1, "Pawn")
					contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(1)(4))
					contents += new BackgroundCase (1, 1)
				case _ =>
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
					contents += new BackgroundCase (1, 1)
			}
		}
	}

	/* Border grid with dead pieces for the black player. */
	class Border0 extends GridPanel (Parameters.nb_case_board + 2, 4) {
		for(i <- 0 to Parameters.nb_case_board + 1) {
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
						contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(0)(0))
						contents += Ksparov.curr_game.promotion_buttons(0)(0)
					case 2 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(0)(1))
						contents += Ksparov.curr_game.promotion_buttons(0)(1)
					case 3 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(0)(2))
						contents += Ksparov.curr_game.promotion_buttons(0)(2)
					case 4 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(0)(3))
						contents += Ksparov.curr_game.promotion_buttons(0)(3)
					case 5 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new NumDeadCase (1, Ksparov.curr_game.dead_pieces(0)(4))
						contents += new DeadCase (0, "Pawn")
					case 6 =>
						contents += new BackgroundCaseWithLabel ((9 - i).toString)
						contents += new BackgroundCase (1, 1)
						contents += new BackgroundCase (1, 1)
						contents += Ksparov.curr_game.play_buttons (0)
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
		Ksparov.curr_game.dead_pieces = Array(new Array[Int](5), new Array[Int](5))
		/* Initilizing the array of cases. */
		DrawBoard.create_grid_cases
		/* For each piece in the game board. */
		for (i <- 0 to game_board.length - 1) {
			/* Transcripting coordinates in one dimension. */
			var coord = game_board(i).pos_x + game_board(i).pos_y * 8
			/* If the piece is alive, update the icon of the case of its position. */
			if (coord >= 0) {
				var piece_path = game_board(i).piece_path
				Ksparov.curr_game.grids(game_board(i).grid)(coord).action.icon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + game_board(i).player.toString + "/" + piece_path)
			/* Else, if the piece is dead, update the array which counts the number of dead piece for each players. */
			} else {
				game_board(i).name match {
					case "queen" => Ksparov.curr_game.dead_pieces(game_board(i).player)(0) += 1
					case "bishop" => Ksparov.curr_game.dead_pieces(game_board(i).player)(1) += 1
					case "knight" => Ksparov.curr_game.dead_pieces(game_board(i).player)(2) += 1
					case "rook" => Ksparov.curr_game.dead_pieces(game_board(i).player)(3) += 1
					case "pawn" => Ksparov.curr_game.dead_pieces(game_board(i).player)(4) += 1
				}
			}
		}
	}

	def enable_promotion (p : Int) {
		Ksparov.curr_game.promotion = true
		for (i <- 0 to 3) {
			Ksparov.curr_game.promotion_buttons(p)(i).enabled = true
			Ksparov.curr_game.promotion_buttons(p)(i).background = Color.red
		}
		Ksparov.frame.contents = new DrawBoard.Board
	}

	def disable_promotion (p : Int) {
		Ksparov.curr_game.promotion = false
		for (i <- 0 to 3) {
			Ksparov.curr_game.promotion_buttons(p)(i).enabled = false
			Ksparov.curr_game.promotion_buttons(p)(i).background = new Color (121, 128, 129)
		}
		draw_game_board (Ksparov.curr_game.board)
		Ksparov.frame.contents = new DrawBoard.Board
		draw_game_messages ("Current_turn", 1 - Ksparov.curr_game.curr_player)
	}

	/* Color in red the reachables cases. */
	def draw_possible_moves (case_list : Array[(Int, Int)], pice_position_x : Int, piece_position_y : Int, grid : Int) {
		/* Coloring the selected case in red. */
		Ksparov.curr_game.grids(grid)(pice_position_x + piece_position_y * 8).background = Color.yellow
		Ksparov.curr_game.grids((grid + 1) % (Ksparov.curr_game.nb_grid))(pice_position_x + piece_position_y * 8).background = Color.yellow

		/* For each reachable case, colors it into red. */
   		for (i <- 0 to case_list.length - 1) {
            var (j, l) = case_list(i)
            Ksparov.curr_game.grids (grid) (j + 8 * l).background = Color.red
            Ksparov.curr_game.grids ((grid + 1) % (Ksparov.curr_game.nb_grid)) (j + 8 * l).background = Color.red
        }
	}

	/* Recolor as "normal" cases : white and black. */
	def clear_possible_moves {
		for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
			for (i <- 0 to 63) {
				if ((i % 8 + i / 8) % 2 == 0) {
					Ksparov.curr_game.grids(k)(i).background = Color.black
				} else {
					Ksparov.curr_game.grids(k)(i).background = Color.white
				}
			}
		}
	}

	/* Draw a given message on the board, the message depends on the argument passed */
	def draw_game_messages (message_type : String, player : Int) {
        var joueur_string = Ksparov.curr_game.game_type match {
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
			case "Current_turn" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "La main est à " + joueur_string +" !"
					Ksparov.curr_game.message_drawer.foreground = Color.black
				case _ => Ksparov.curr_game.message_drawer.text = "La main est au " + joueur_string +" !"
					Ksparov.curr_game.message_drawer.foreground = Color.black
			}

			/* Draw if a player is in check. */
			case "Check" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = joueur_string + " est en échec !"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "Le " + joueur_string + " est en échec !"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}

			case "Time" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Perte au temps,<br>" + joueur_string + " gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			/* Draw if a player is mate. */
			case "Mate" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>" + (if (player == 0) {Load.infos("White")} else {Load.infos("Black")}) + " gagne la partie !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>le "+ (if(player == 0){"joueur blanc"}else{"joueur noir"}) + " gagne la partie !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}

			/* Draw if an AI cannot move (this option has only been implemented for IA). */
			case "Pat" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>"+ joueur_string +" ne peut plus bouger !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>le "+ joueur_string +" ne peut plus bouger !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}
			/* Draw if there is no more checkmate possible */
			case "Nulle" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Partie nulle : "+ joueur_string +" ne peut plus mater !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Partie nulle : le "+ joueur_string +" ne peut plus mater !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}
			/* Draw if 50 moves has been made without a pawn move or a piece taken */
			case "50coups" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 50 coups sans prise ni mouvement de pion ! </html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red
			/*Draw if a single position occured 3 times in the same game */
			case "TripleRepetition" => 	Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 3 répétitions de la même position ! </html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			case "Promotion" => Ksparov.curr_game.curr_player match {
				case 0 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion noir !"
				case 1 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion blanc !"
			}

        	case "1-0" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>" + Load.infos("White") +" gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "0-1" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>"+ Load.infos("Black") +" gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "1/2-1/2" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "*" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Partie non finie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "!$" => Ksparov.curr_game.message_drawer.text = "Bon coup de " + joueur_string +" !"
				Ksparov.curr_game.message_drawer.foreground = Color.black
        	case "!!" => Ksparov.curr_game.message_drawer.text = "Trés bon coup de " + joueur_string +" !"
				Ksparov.curr_game.message_drawer.foreground = Color.black
        	case "?$" =>Ksparov.curr_game.message_drawer.text = "Que fait " + joueur_string +" ?"
				Ksparov.curr_game.message_drawer.foreground = Color.black
        	case "??" => Ksparov.curr_game.message_drawer.text = "Coup trés surprenant de la part de " + joueur_string +" !"
				Ksparov.curr_game.message_drawer.foreground = Color.black
        	case "!?" => Ksparov.curr_game.message_drawer.text =  joueur_string +" nous cache-t-il quelque chose ?"
				Ksparov.curr_game.message_drawer.foreground = Color.black
        	case "?!" => Ksparov.curr_game.message_drawer.text = "Qu'espère  " + joueur_string + " en jouant ce coup ?"
				Ksparov.curr_game.message_drawer.foreground = Color.black
		}
	}
}
