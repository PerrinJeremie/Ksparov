import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.matching.Regex

/* This file is organised in objects, each of then draw a certain windows.
   To change the application window, we juste change the contents of Kasparov.frame in game.scala. */

/* The exception of the above rule : this class defines a grid of background cases,
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
				icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
			}
		}
	}
}

/* Draw the main menu of the application. A menu is an instance of DrawMenu.Menu. */
object DrawMenu {
	val nb_menu_option = 6

	/* The generic class for the buttons, the role of a particular is defined
	   depending on the parameter passed to the class. */
	class Option (name : String) extends Button {
		font = Constants.text_font
		preferredSize = Constants.dim_big
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = Action (name) {
			name match {
				case "Jouer une partie" => Ksparov.frame.contents = new DrawGameSelection.Menu
				case "Voir les règles du jeu" => Ksparov.frame.contents = new DrawNotYet.NotYet ("Revenir au menu")
				case "Charger une partie" => Ksparov.frame.contents = new DrawNotYet.NotYet ("Revenir au menu")
				case "Voir les scores" => Ksparov.frame.contents = new DrawNotYet.NotYet ("Revenir au menu")
				case "Gérer les paramètres" => Ksparov.frame.contents = new DrawParameters.Parameters
				case "Quitter Ksparov" => Ksparov.frame.dispose()
			}
		}
	}

	/* The principal menu : for each line there are two buttons and a background between them. */
	class MenuGrid extends GridPanel (nb_menu_option + 1, 3) {
    	for( i <- 0 to nb_menu_option) {
    		if (i % 2 == 0) {
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
    		} else { i match {
	        case 1 => contents += new Option ("Jouer une partie")
			contents += new BackgroundCase (1, 3)
        	contents += new Option ("Charger une partie")
        	case 3 => contents += new Option ("Voir les scores")
        	contents += new BackgroundCase (1, 3)
        	contents += new Option ("Voir les règles du jeu")
        	case 5 => contents += new Option ("Gérer les paramètres")
        	contents += new BackgroundCase (1, 3)
        	contents += new Option ("Quitter Ksparov")
    		}}
    	}
	}

	/* The final appearance with the principal menu between two columns of background cases. */
	class Menu extends BorderPanel {
    	layout (new BackgroundCase (nb_menu_option + 1, 1)) = East
    	layout (new BackgroundCase (nb_menu_option + 1, 1)) = West
    	layout (new MenuGrid) = Center
	}
}

/* An object no so useful, it draws a message telling the users that functionnality has not been programmed.
   We use it for keeping place to the future upgrades of the game. */
object DrawNotYet {

	/* The button to come back where you were in the application menu or in a game. */
	class ComeBack (name : String) extends Button {
		preferredSize = Constants.dim_big
		minimumSize = Constants.dim_big
		maximumSize = Constants.dim_big
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = Action (name) {
			name match {
				case "Revenir au menu" => Ksparov.frame.contents = new DrawMenu.Menu
				case "Revenir à la partie" => Ksparov.frame.contents = new DrawBoard.Board
			}
		}
	}

	/* The window is basic a message and the come back button. */
	class CenterGrid (name : String) extends GridPanel (5,1) {
		for (i <- 0 to 4) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 3)
			} else { i match {
				case 1 => contents += new Label {
					font = Constants.text_font
					/* The beauty of scala : using HTML langage to format text and to have a carriage return (\n does not work...) */
					text = "<html><div style='text-align : center;'>Cette fonctionnalité n'est pas<br>encore développée, veuillez<br>patienter quelque peu !</div></html>"
				}
				case 3 => contents += new ComeBack (name)
			}}
		}
	}

	/* A simple layout : the center grid between two columns of background cases. */
	class NotYet (name : String) extends BorderPanel {
		layout (new BackgroundCase (5, 1)) = East
		layout (new BackgroundCase (5, 1)) = West
		layout (new CenterGrid (name)) = Center
	}
}

/* The window to choose and modify the parameters of the application : type of texture and type of pieces.
   Every parameter is recorded in the src/main/resources/Parameters file so they are kept from one gmae to the other.
   Other options will be added soon. */
object DrawParameters {

	val nb_option_max = 5

	/* Buttons for the texture choice, the parameter defines which texture the button stands for. */
	class TextureOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		/* The border is red for the actual parameter and black for other. The regural expression
		   get the number in the texture path. */
		if ((Constants.pattern findAllIn Constants.small_texture_path).mkString.toInt == number) {
			border = new javax.swing.border.LineBorder (Color.red, 3)
		} else {
			border = new javax.swing.border.LineBorder (Color.black, 3)
		}
		action = new Action("") {
			/* The action is different for other button because to have an icon on a button, you have to put in the action. */
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Texture_small_" + number.toString + ".png")
			def apply = {
				/* When the button is pushed, we write the new parameters in the src/main/resources/Parameters file.
				   After doing this, we apply the new parameters to that choice is dynamic. */
				Constants.write_parameters ((Constants.pattern findAllIn Constants.pieces_path).mkString, number.toString)
				Constants.apply_parameters
				Ksparov.frame.contents = new DrawParameters.Parameters
			}
		}
	}

	/* Button fot the piece type choice, excatly the same as before. */
	class PieceOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		if ((Constants.pattern findAllIn Constants.pieces_path).mkString.toInt == number) {
			border = new javax.swing.border.LineBorder (Color.red, 3)
		} else {
			border = new javax.swing.border.LineBorder (Color.black, 3)
		}
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Pieces/" + number.toString + "/1/King.png")
			def apply {
				Constants.write_parameters (number.toString, (Constants.pattern findAllIn Constants.small_texture_path).mkString (","))
				Constants.apply_parameters
				Ksparov.frame.contents = new DrawParameters.Parameters
			}
		}
	}

	/* The menu for texture : an alternance of background cases and texture option. */
	class TextureGrid extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 1 to 2 * nb_option_max - 1) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new TextureOption (Math.round(i / 2) + 1)
			}
		}
	}

	/* The menu for pieces : an alternance of background cases and pieces option. */
	class PiecesGrid extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 1 to 2 * nb_option_max - 1) {
			if (i % 2 == 0 || i > 4) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new PieceOption (Math.round(i / 2) + 1)
			}
		}
	}

	/* The final menu with the texture choice first then the piece choice and finally a come back button. */
	class CenterGrid extends GridPanel (9, 1) {
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
		contents += new Label ("Choissisez le fond") {
			font = Constants.text_font
			border = new javax.swing.border.LineBorder (Color.black, 2)
			background = new Color (200, 200, 200)
			opaque = true}
		contents += new TextureGrid
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
		contents += new Label ("Choissisez le type de pièces") {
			font = Constants.text_font
			border = new javax.swing.border.LineBorder (Color.black, 2)
			background = new Color (200, 200, 200)
			opaque = true}
		contents += new PiecesGrid
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
		contents += new Button {
			action = Action("<html>Appliquer les changements<br>et revenir au menu</html>") {Ksparov.frame.contents = new DrawMenu.Menu}
			border = new javax.swing.border.LineBorder (Color.black, 2)}
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
	}

	/* The final window with borders on each side. */
	class Parameters extends BorderPanel {
		layout (new BackgroundCase (9, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (9, 1)) = West
	}
}

/* This object is for the game selection : Human vs Human, AI vs AI or AI vs Human. */
object DrawGameSelection {

	/* The button for the choice selection : change the value of Constants.game_type when pressed. */
	class Option (name : String, num : Int) extends Button {
		preferredSize = Constants.dim_big
		minimumSize = Constants.dim_big
		maximumSize = Constants.dim_big
		font = Constants.text_font
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = Action (name) {
			Constants.game_type = num
			/* Launched a game. */
		    Ksparov.init_game(num)
    		Ksparov.frame.contents = new DrawBoard.Board
		}
	}

	/* The menu with options and a come back button. */
	class CenterGrid extends GridPanel (9, 1) {
		for (i <- 0 to 8) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 3)
			} else { i match {
				case 1 => contents += new Option ("Humain vs Humain", 1)
				case 3 => contents += new Option ("Humain vs IA", 2)
				case 5 => contents += new Option ("IA vs IA", 3)
				case 7 => contents += new Button {
					font = Constants.text_font
					border = new javax.swing.border.LineBorder (Color.black, 2)
					action = Action ("Revenir au menu") {Ksparov.frame.contents = new DrawMenu.Menu}}
			}}
		}
	}

	/* The final class with background. */
	class Menu extends BorderPanel {
		layout (new BackgroundCase (9, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (9, 1)) = West
	}
}

/* The most important class : draw the board itself ! */
object DrawBoard {

	/* We need here background cases with label in order to have letters and numbers around the board. */
	class BackgroundCaseWithLabel (label : String) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
		text = label
		/* This option make the superposition of text and icon possible. */
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
	}

	/* These cases just have an icon a piece (except king), they are on the left and right side of the bord
	    and they are used to count the number of each dead piece for each player. */
	class DeadCase (player : Int, piece : String) extends Label {
		preferredSize = Constants.dim_small
		/* They are colored in a grey close to the lead color. */
		background = new Color (121, 128, 129)
		/* By default labels are not opaque at all, giving them no color... */
		opaque = true
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player.toString + "/" + piece + ".png")
		border = new javax.swing.border.LineBorder (Color.black, 1)

	}

	/* These cases are the just next to the previous one, they are numbers of dead pieces. */
	class NumDeadCase (player : Int, number : Int) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
		font = Constants.num_dead_font
		text = number.toString
	}

	/* The case of the board, definend by the coordinates. They are alternatively black and white
	   depending on their position. They go from (0, 0) to (7, 7). */
	class Case (x : Int, y : Int) extends Button {
		preferredSize = Constants.dim_small
		if ((x + y) % 2 == 0) {
			background = Color.black
		} else {
			background = Color.white
  		}
		action = new Action ("") {
			def apply = {
				/* When we click one a case, Constants.selected_case receive it in a one dimension way.
				   Then we launched the movment method in game.scala. */
				Constants.selected_case = x + y * 8
                Ksparov.play_move(x,y)
			}
		}
	}

	/* The board with its 63 cases, it is represented as an Array in Constants.grid_cases,
	   we need it in an array to change the icon variable depending on board.
	   The dimension is now in one dimension because mutli dimension array in scala are not well supported. */
	def create_grid_cases {
		for (i <- 0 to Constants.nb_case_board - 1) {
			for (j <- 0 to Constants.nb_case_board - 1) {
				Constants.grid_cases (i + j * 8) = new Case (i, j)
			}
		}
	}

	/* The center grid with the board and background with label around. */
	class Grid extends GridPanel (Constants.nb_case_board + 2, Constants.nb_case_board + 2) {
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
				contents += new BackgroundCase (1, 1)
			} else {
				for (j <- -1 to Constants.nb_case_board) {
					if (j == -1 || j == Constants.nb_case_board) {
						contents += new BackgroundCaseWithLabel ((8 - i).toString)
					} else {
						/* Using 7 - i here because we want "classic" axis from left to right and from bottom to top. */
						contents += Constants.grid_cases (j + (7 - i) * 8)
					}
				}
			}
		}
	}

	/* The board has a menu on his top defined here. */
	class Header extends GridPanel (2, 2) {
		contents += new Button {
		font = Constants.text_font
			action = Action ("Recommencer une partie") {
	    		Ksparov.frame.contents = new DrawGameSelection.Menu
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Sauvegarder la partie") {
                Save.write_to_file("Essais_de_Sauvegarde","ENChess","Cachan","AAAA.MM.JJ", "1","Jérémie","Jérémie","*") 
				Ksparov.frame.contents = new DrawNotYet.NotYet ("Revenir à la partie")
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Revenir au menu principal") {
				Ksparov.frame.contents = new DrawMenu.Menu
			}
		}
		contents += new Button {
		font = Constants.text_font
			action = Action ("Quitter Ksparov") {
	    		Ksparov.frame.dispose()
			}
		}
	}

	/* Behind the board, there is the message drawer, which is a label where messages like "check", "mat"... are displayed. */
	class MessageDrawer (message : String) extends Label {
		font = Constants.text_font
		preferredSize = Constants.dim_message_drawer
		background = new Color (220, 220, 220)
		opaque = true
		foreground = Color.black
		/* The text is defined by its parameter, which is contained in Constants.message_drawer. */
		text = message
		horizontalTextPosition = Alignment.Center
		verticalTextPosition = Alignment.Center
		border = new javax.swing.border.LineBorder (Color.black, 1)
	}

	/* The footer of the window with the message drawer. */
	class Footer extends BorderPanel {
		layout(new BackgroundCase (1, 5)) = East
		layout(new BackgroundCase (1, 5)) = West
		layout(Constants.message_drawer) = Center
	}

	/* Border grid with dead pieces for the white player. */
	class Border1 extends GridPanel (Constants.nb_case_board + 2, 3) {
		for(i <- 0 to Constants.nb_case_board + 1) {
			if (i < 4 || i > 8) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else { i match {
				case 4 =>
					contents += new DeadCase (1, "Queen")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(0))
					contents += new BackgroundCase (1, 1)
				case 5 =>
					contents += new DeadCase (1, "Bishop")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(1))
					contents += new BackgroundCase (1, 1)
				case 6 =>
					contents += new DeadCase (1, "Knight")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(2))
					contents += new BackgroundCase (1, 1)
				case 7 =>
					contents += new DeadCase (1, "Rook")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(3))
					contents += new BackgroundCase (1, 1)
				case 8 =>
					contents += new DeadCase (1, "Pawn")
					contents += new NumDeadCase (1, Constants.dead_pieces(1)(4))
					contents += new BackgroundCase (1, 1)
				}
			}
		}
	}

	/* Border grid with dead pieces for the black player. */
	class Border0 extends GridPanel (Constants.nb_case_board + 2, 2) {
		for(i <- 0 to Constants.nb_case_board + 1) {
			if (i < 1 || i > 5) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else { i match {
				case 1 =>
					contents += new BackgroundCase (1, 1)
					contents += new NumDeadCase (1, Constants.dead_pieces(0)(0))
					contents += new DeadCase (0, "Queen")
				case 2 =>
					contents += new BackgroundCase (1, 1)
					contents += new NumDeadCase (1, Constants.dead_pieces(0)(1))
					contents += new DeadCase (0, "Bishop")
				case 3 =>
					contents += new BackgroundCase (1, 1)
					contents += new NumDeadCase (1, Constants.dead_pieces(0)(2))
					contents += new DeadCase (0, "Knight")
				case 4 =>
					contents += new BackgroundCase (1, 1)
					contents += new NumDeadCase (1, Constants.dead_pieces(0)(3))
					contents += new DeadCase (0, "Rook")
				case 5 =>
					contents += new BackgroundCase (1, 1)
					contents += new NumDeadCase (1, Constants.dead_pieces(0)(4))
					contents += new DeadCase (0, "Pawn")
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

	//FAIS MOI CETTE FONCTION SIMON FDP//
	def draw_promotion () : String = {
		"queen"
	}

	/* Draw a game board (an array of pieces) on the chessboard. */
	def draw_game_board (game_board : Array[Piece]) {
		var coord = 0
		var player_path = ""
		var piece_path = ""
		/* Reinitializing the dead piece array to avoid mutli-counting. */
		Constants.dead_pieces = Array(new Array[Int](5), new Array[Int](5))
		/* Initilizing the array of cases. */
		DrawBoard.create_grid_cases
		/* For each piece in the game board. */
		for (i <- 0 to game_board.length - 1) {
			/* Transcripting coordinates in one dimension. */
			coord = game_board(i).pos_x + game_board(i).pos_y * 8
			/* If the piece is alive, update the icon of the case of its position. */
			if (coord >= 0) {
				piece_path = game_board(i).piece_path
				Constants.grid_cases(coord).icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + game_board(i).player.toString + "/" + piece_path)
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
		Ksparov.frame.contents = new DrawBoard.Board
	}

	/* Color in red the reachables cases. */
	def draw_possible_moves (case_list : Array[(Int, Int)], pice_position_x : Int, piece_position_y : Int) {
		/* Coloring the selected case in red. */
		Constants.grid_cases(pice_position_x + piece_position_y * 8).background = Color.red
		/* For each reachable case, colors it into red. */
        for (i <- 0 to case_list.length - 1) {
            var (k, l) = case_list(i)
            Constants.grid_cases(k + 8 * l).background = Color.red
        }
	}

	/* Recolor as "normal" cases : white and black. */
	def clear_possible_moves () {
		for (i <- 0 to 63) {
			if ((i % 8 + i / 8) % 2 == 0) {
				Constants.grid_cases(i).background = Color.black
			} else {
				Constants.grid_cases(i).background = Color.white
			}
		}
	}

	/* Draw a given message on the board, the message depends on the argument passed */
	def draw_messages (message_type : String) {
		message_type match {

			/* Draw who's player the turn is. */
			case "Current_turn" => Constants.curr_player match {
				case 0 => Constants.message_drawer.text = "La main est au joueur blanc !"
					Constants.message_drawer.foreground = Color.black
				case 1 => Constants.message_drawer.text = "La main est au joueur noir !"
					Constants.message_drawer.foreground = Color.black
			}

			/* Draw if a player is in check. */
			case "Check" => Constants.curr_player match {
				case 0 => Constants.message_drawer.text = "Le joueur blanc est en échec !"
					Constants.message_drawer.foreground = Color.red
				case 1 => Constants.message_drawer.text = "Le joueur noir est en échec !"
					Constants.message_drawer.foreground = Color.red
			}

			/* Draw if a player is mate. */
			case "Mate" => Constants.curr_player match {
				case 0 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>le joueur noir gagne la partie !</html>"
					Constants.message_drawer.foreground = Color.red
				case 1 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>le joueur blanc gagne la partie !</html>"
					Constants.message_drawer.foreground = Color.red
			}

			/* Draw if an AI cannot move (this option has only been implemented for IA). */
			case "Pat" => Constants.curr_player match {
				case 0 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>l'IA noire ne peut plus bouger !</html>"
					Constants.message_drawer.foreground = Color.red
				case 1 => Constants.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>l'IA blanche ne peut plus bouger !</html>"
					Constants.message_drawer.foreground = Color.red
			}
		}
	}
}
