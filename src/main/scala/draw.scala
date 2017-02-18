import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.matching.Regex

/*J'ai organisé ce fichier en plusieurs objets selon ce qu'ils
dessinent : Menu, Board ou Action.*/

class BackgroundCase (i : Int, j : Int) extends GridPanel (i, j) {
	for(k <- 0 to i - 1) {
		for(l <- 0 to j - 1) {
			contents += new Label {
				preferredSize = Constants.dim_small
				icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
			}
		}
	}
}

/*Objet qui définit toutes les classes nécessaires pour 
construire le menu. Si on veut charger le menu dans l'application,
il faut créer une instance de la classe Menu*/
object DrawMenu {
	val nb_menu_option = 6

	class Option extends Button { 
		preferredSize = Constants.dim_big
		border = new javax.swing.border.LineBorder (Color.black, 2)
	}

	class Play_button extends Option {
    	action = Action ("Jouer une partie") {
    		Ksparov.frame.contents = new DrawGameSelection.Menu
    	}
	}

	class Rules_button extends Option {
    	action = Action ("Voir les règles du jeu") {
    		Ksparov.frame.contents = new DrawNotYet.NotYet
    	}
	}

	class Load_button extends Option {
    	action = Action ("Charger une partie") {
    		Ksparov.frame.contents = new DrawNotYet.NotYet      
		}
	}

	class Score_button extends Option {
    	action = Action ("Voir les scores") {
    		Ksparov.frame.contents = new DrawNotYet.NotYet      
    	}
	}

	class Option_button extends Option {
    	action = Action ("Gérer les paramètres") {
    		Ksparov.frame.contents = new DrawParameters.Parameters
	    }
	}

	class Exit_button extends Option {
    	action = Action ("Quitter Ksparov") {
    		Ksparov.frame.dispose()
    	}
	}

	class MenuGrid extends GridPanel (nb_menu_option + 1, 3) {
    	for( i <- 0 to nb_menu_option) {
    		if (i % 2 == 0) {
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
    		} else { i match {
	        case 1 => contents += new Play_button
			contents += new BackgroundCase (1, 3)
        	contents += new  Load_button
        	case 3 => contents += new Score_button
        	contents += new BackgroundCase (1, 3)
        	contents += new Rules_button
        	case 5 => contents += new Option_button
        	contents += new BackgroundCase (1, 3)
        	contents += new Exit_button
    		}}
    	}
	}

	class Menu extends BorderPanel {
    	layout (new BackgroundCase (nb_menu_option + 1, 1)) = East
    	layout (new BackgroundCase (nb_menu_option + 1, 1)) = West
    	layout (new MenuGrid) = Center
	}
}

object DrawNotYet {

	class CenterGrid extends GridPanel (5,1) {
		for (i <- 0 to 4) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 3)
			} else { i match {
				case 1 => contents += new Label {
					text = "<html>Cette fonctionnalité n'est pas<br>encore développée, veuillez<br>patienter quelque peu !</html>"
				}
				case 3 => contents += new Button {
					action = Action("Revenir au menu") {
						Ksparov.frame.contents = new DrawMenu.Menu
					}
				}
			}}
		}
	}

	class NotYet extends BorderPanel {
		layout (new BackgroundCase (5, 1)) = East
		layout (new BackgroundCase (5, 1)) = West
		layout (new CenterGrid) = Center
	}
}

object DrawParameters {
	val nb_option_max = 5
	
	class TextureOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		border = new javax.swing.border.LineBorder (Color.black, 3)
		action = new Action("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Texture_small_" + number.toString + ".png")
			def apply = {
				Constants.write_parameters ((Constants.pattern findAllIn Constants.pieces_path).mkString (","), number.toString)
				Constants.apply_parameters 
				Ksparov.frame.contents = new DrawParameters.Parameters
			}
		}
	}

	class PieceOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		border = new javax.swing.border.LineBorder (Color.black, 3)
		action = new Action ("") {
			icon = new javax.swing.ImageIcon(Constants.resources_path + "Pieces/" + number.toString + "/1/King.png")
			def apply {
				Constants.write_parameters (number.toString, (Constants.pattern findAllIn Constants.small_texture_path).mkString (","))
				Constants.apply_parameters
			}
		}
	}

	class TextureGrid extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 1 to 2 * nb_option_max - 1) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new TextureOption (Math.round(i / 2) + 1)
			}
		}
	}

	class PiecesGrid extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 1 to 2 * nb_option_max - 1) {
			if (i % 2 == 0 || i > 4) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new PieceOption (Math.round(i / 2) + 1)
			}
		}
	}

	class CenterGrid extends GridPanel (9, 1) {
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
		contents += new Label ("Choissisez le fond") {
			border = new javax.swing.border.LineBorder (Color.black, 2)
			background = new Color (200, 200, 200)
			opaque = true}
		contents += new TextureGrid
		contents += new BackgroundCase (1, 2 * nb_option_max - 1)
		contents += new Label ("Choissisez le type de pièces") {
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

	class Parameters extends BorderPanel {
		layout (new BackgroundCase (9, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (9, 1)) = West
	}
}

object DrawGameSelection {

	class Option (name : String, num : Int) extends Button {
		preferredSize = Constants.dim_big
		border = new javax.swing.border.LineBorder (Color.black, 2)
		action = Action (name) {
			Constants.game_type = num
		    Ksparov.init_game(num)
    		Ksparov.frame.contents = new DrawBoard.Board	
		}
	}

	class CenterGrid extends GridPanel (9, 1) {
		for (i <- 0 to 8) {
			if (i % 2 == 0) {
				contents += new BackgroundCase (1, 3)
			} else { i match {
				case 1 => contents += new Option ("Humain vs Humain", 1)
				case 3 => contents += new Option ("Humain vs IA", 2)
				case 5 => contents += new Option ("IA vs IA", 3)
				case 7 => contents += new Button {
					border = new javax.swing.border.LineBorder (Color.black, 2)
					action = Action ("Revenir au menu principal") {Ksparov.frame.contents = new DrawMenu.Menu}}
			}}
		}	
	}

	class Menu extends BorderPanel {
		layout (new BackgroundCase (9, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (9, 1)) = West
	}	
}

/*Objet qui permet de dessiner le plateau, la classe pour le plateau entier est Board.*/
object DrawBoard {

	class BackgroundCaseWithLabel(label : String) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
		text = label
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
	}

	class DeadCase (player : Int, piece : String) extends Label {
		preferredSize = Constants.dim_small
		background = new Color (121, 128, 129)
		opaque = true
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player.toString + "/" + piece + ".png")
		border = new javax.swing.border.LineBorder (Color.black, 1)

	}

	class NumDeadCase (player : Int, number : Int) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
		horizontalTextPosition = Alignment.Center
		foreground = Constants.text_color
		font = new Font("Arial", 0, 25)
		text = number.toString
	}

	/*Classe des cases, attention les cases commençent en 0,0 et finisse en 7,7,
	pour la suite, j'ai utilisé un seul entier pour référencer les cases, une case
	de coordonnées (x, y) est à la position 8*x+y*/
	class Case (x : Int, y : Int) extends Button {
		preferredSize = Constants.dim_small
		if ((x + y) % 2 == 0) {
			background = Color.black
		} else {
			background = Color.white
  		}
		action = new Action ("") {
			def apply = {
				Constants.selected_case = x + y * 8
				background = Color.red
                Ksparov.play_move(x,y)
			}
		}
	}

	def create_grid_cases {
		for (i <- 0 to Constants.nb_case_board - 1) {
			for (j <- 0 to Constants.nb_case_board - 1) {
				Constants.grid_cases (i + j * 8) = new Case (i, j)
			}
		}
	}

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
						contents += new BackgroundCaseWithLabel ((i+1).toString)
					} else {
						/* Using 7 - i here because we want "classic" axis from left to right and from bottom to top */
						contents += Constants.grid_cases (j + (7 - i) * 8)
					}
				}
			}
		}
	}

	class Header extends GridPanel (2, 2) {
		contents += new Button {
			action = Action ("Recommencer une partie") {
	    		Ksparov.frame.contents = new DrawGameSelection.Menu
			}
		}
		contents += new Button {
			action = Action ("Sauvegarder la partie") {
				Ksparov.frame.contents = new DrawNotYet.NotYet
			}
		}
		contents += new Button {
			action = Action ("Revenir au menu principal") {
				Ksparov.frame.contents = new DrawMenu.Menu
			}
		}
		contents += new Button {
			action = Action ("Quitter Ksparov") {
	    		Ksparov.frame.dispose()
			}
		}
	}

	class MessageDrawer (message : String) extends Label {
		preferredSize = new Dimension (400, 80)
		background = new Color (220, 220, 220)
		opaque = true
		foreground = Color.black
		text = message
		border = new javax.swing.border.LineBorder (Color.black, 1)
	}

	class Footer extends BorderPanel {
		layout(new BackgroundCase (1, 5)) = East
		layout(new BackgroundCase (1, 5)) = West
		layout(Constants.message_drawer) = Center
	}

	class Border1 extends GridPanel (Constants.nb_case, 3) {
		for(i <- 0 to Constants.nb_case - 1) {
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

	class Border0 extends GridPanel (Constants.nb_case, 2) {
		for(i <- 0 to Constants.nb_case - 1) {
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

	class Board extends BorderPanel {
		layout(new Header) = North
		layout(new Border1) = West
		layout(new Border0) = East
		layout(new Footer) = South
		layout(new Grid) = Center
	}
}

/*Objet comprenant les méthodes pour dessiner des actions : mouvement des pièces, etc !*/
object DrawActions {

	def draw_game_board (game_board : Array[Piece]) {
		var coord = 0
		var player_path = ""
		var piece_path = ""
		/*Reinitializing the dead piece array */
		Constants.dead_pieces = Array(new Array[Int](5), new Array[Int](5))
		DrawBoard.create_grid_cases
		for (i <- 0 to game_board.length - 1) {
			coord = game_board(i).pos_x + game_board(i).pos_y * 8
			if (coord >= 0) {
				game_board(i).name match {
					case "pawn" => piece_path = "Pawn.png"
					case "queen" => piece_path = "Queen.png"
					case "king" => piece_path = "King.png"
					case "rook" => piece_path = "Rook.png"
					case "knight" => piece_path = "Knight.png"
					case "bishop" => piece_path = "Bishop.png"
				}
				Constants.grid_cases(coord).icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + game_board(i).player.toString + "/" + piece_path)
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
		Ksparov.frame.contents = new DrawBoard.Board
	}

	def draw_possible_moves (case_list : Array[(Int, Int)], pice_position_x : Int, piece_position_y : Int) {
		Constants.grid_cases(pice_position_x + piece_position_y * 8).background = Color.red
        for (i <- 0 to case_list.length - 1) {
            var (k, l) = case_list(i)
            Constants.grid_cases(k + 8 * l).background = Color.red
        }
	}

	def clear_possible_moves () {
		for (i <- 0 to 63) {
			if ((i % 8 + i / 8) % 2 == 0) {
				Constants.grid_cases(i).background = Color.black
			} else {
				Constants.grid_cases(i).background = Color.white
			}
		}
	}

	/* Draw a given message on the board, the type of message is : 0 for the initial message 
	   1 for who have to play, 2 if there is check and 3 for mate */
	def draw_messages (message_type : Int) {
		message_type match {
			case 0 => Constants.message_drawer.text = "Bienvenu dans Ksparov, les blancs commençent la partie !"
					Constants.message_drawer.foreground = Color.black
			case 1 => Switches.curr_player match {
				case 0 => Constants.message_drawer.text = "La main est au joueur blanc !"
					Constants.message_drawer.foreground = Color.black
				case 1 => Constants.message_drawer.text = "La main est au joueur noir !"
					Constants.message_drawer.foreground = Color.black
			}
			case 2 => Switches.curr_player match {
				case 0 => Constants.message_drawer.text = "Le joueur noir est en échec !"
					Constants.message_drawer.foreground = Color.red
				case 1 => Constants.message_drawer.text = "Le joueur blanc est en échec !"
					Constants.message_drawer.foreground = Color.red
			}
			case 3 => Switches.curr_player match {
				case 0 => Constants.message_drawer.text = "Echec et mat, le joueur noir gagne la partie !"
					Constants.message_drawer.foreground = Color.red
				case 1 => Constants.message_drawer.text = "Echec et mat, le joueur blanc gagne la partie !"
					Constants.message_drawer.foreground = Color.red
			}
		}
	}
}
