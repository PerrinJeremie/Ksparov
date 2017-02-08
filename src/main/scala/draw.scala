import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._

/*J'ai organisé ce fichier en plusieurs objets selon ce qu'ils
dessinent : Menu, Board ou Action.*/

/*Objet qui définit toutes les classes nécessaires pour 
construire le menu. Si on veut charger le menu dans l'application,
il faut créer une instance de la classe Menu*/
object DrawMenu {
	val nb_menu_option = 6

	class Option extends Button { 
		preferredSize = Constants.dim_big
	}

	class Play_button extends Option {
    	action = Action ("Jouer une partie") {
    		Ksparov.frame.contents = new DrawBoard.Board
    	}
	}

	class Resume_button extends Option {
    	action = Action ("Reprendre la dernière partie") {
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

	class BigBackground extends Label {
    	preferredSize = Constants.dim_big
    	icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.big_texture_path)
	}

	class SmallBackground extends Label {
    	preferredSize = Constants.dim_small
    	icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
	}

	class MenuGrid extends GridPanel (nb_menu_option + 1, 3) {
    	for( i <- 0 to nb_menu_option) {
    		if (i % 2 == 0) {
        		contents += new BigBackground
        		contents += new BigBackground
        		contents += new BigBackground
    		} else { i match {
	        case 1 => contents += new Resume_button
			contents += new BigBackground
        	contents += new Play_button 
        	case 3 => contents += new Load_button
        	contents += new BigBackground
        	contents += new Score_button 
        	case 5 => contents += new Option_button
        	contents += new BigBackground 
        	contents += new Exit_button
    		}}
    	}
	}

	class BorderGrid extends GridPanel (nb_menu_option + 1, 1) {
    	for( i <- 0 to nb_menu_option) {
    		contents += new SmallBackground
    	}
	}

	class Menu extends BorderPanel {
    	layout (new BorderGrid) = East
    	layout (new BorderGrid) = West
    	layout (new MenuGrid) = Center
	}
}

object DrawNotYet {

	class BigBackground extends Label {
    	preferredSize = Constants.dim_big
    	icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.big_texture_path)
	}

	class SmallBackground extends Label {
    	preferredSize = Constants.dim_small
    	icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
	}

	class CenterGrid extends GridPanel (5,1) {
		for (i <- 0 to 4) {
			if (i % 2 == 0) {
				contents += new BigBackground
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

	class BorderGrid extends GridPanel (5, 1) {
		for (i <- 0 to 4) {
			contents += new SmallBackground
		}
	}

	class NotYet extends BorderPanel {
		layout (new BorderGrid) = East
		layout (new BorderGrid) = West
		layout (new CenterGrid) = Center
	}
}

object DrawParameters {
	val nb_option_max = 2

	class SmallBackground extends Label {
    	preferredSize = Constants.dim_small
    	icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
	}

	class BigBackground extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 0 to 2 * nb_option_max - 2) {
			contents += new SmallBackground
		}
	}

	class TextureOption (number : Int) extends Button {
		preferredSize = Constants.dim_small
		opaque = true
		background = Color.black
		icon = new javax.swing.ImageIcon(Constants.resources_path + "Wood_Texture_small_" + number.toString + ".png")
		action = Action ("") {
			println (Constants.resources_path + "Wood_Texture_small_" + number.toString + ".png")
			Constants.small_texture_path = "Wood_Texture_small_" + number.toString + ".png"
			Constants.big_texture_path = "Wood_Texture_big_" + number.toString + ".png"
			println (Constants.resources_path + "Wood_Texture_big_" + number.toString + ".png")
			println (Constants.resources_path + "Wood_Texture_small_" + number.toString + ".png")
		}
	}

	class TextureGrid extends GridPanel (1, 2 * nb_option_max - 1) {
		for( i <- 1 to 2 * nb_option_max - 1) {
			if (i % 2 == 0) {
				contents += new SmallBackground
			} else {
				contents += new TextureOption (Math.round(i / 2) + 1)
			}
		}
		
	}

	class CenterGrid extends GridPanel (6, 1) {
		contents += new BigBackground
		contents += new Label ("Choissisez le fond")
		contents += new TextureGrid
		contents += new BigBackground
		contents += new Button (Action("<html>Appliquer les changements<br>et revenir au menu</html>") {Ksparov.frame.contents = new DrawMenu.Menu})
		contents += new BigBackground
	}

	class BorderGrid extends GridPanel (6, 1) {
		for (i <- 0 to 5) {
			contents += new SmallBackground
		}
	}

	class Parameters extends BorderPanel {
		layout (new BorderGrid) = East
		layout (new CenterGrid) = Center
		layout (new BorderGrid) = West
	}
}

/*Objet qui permet de dessiner le plateau, la classe pour le plateau entier est Board.*/
object DrawBoard {

	class BorderCase (x : Int, y : Int) extends Label {
  		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.big_texture_path)
  		preferredSize = Constants.dim_small
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
		action = Action ("") {
			Constants.selected_case = x * 8 + y
			println("Click case nb : " + Constants.selected_case.toString)
		}
	}

	var grid_cases = new Array[Case](Constants.nb_case_board * Constants.nb_case_board) 
	for (i <- 0 to Constants.nb_case_board - 1) {
		for( j <- 0 to Constants.nb_case_board - 1) {
			grid_cases(i * 8 + j) = new Case(i, j)
		}
	}

	class Grid extends GridPanel (Constants.nb_case, Constants.nb_case) {
		for (i <- 0 to Constants.nb_case - 1) {
			for (j <- 0 to Constants.nb_case - 1) {
				if (i < Constants.nb_case_border || i > Constants.nb_case - Constants.nb_case_border - 1 ||
				j < Constants.nb_case_border || j > Constants.nb_case - Constants.nb_case_border - 1) {
					contents += new BorderCase (i - Constants.nb_case_border, j - Constants.nb_case_border)
				} else {
					contents += grid_cases((i - Constants.nb_case_border) * 8 + j - Constants.nb_case_border)
				}
			}
		}
	}

	class Header extends GridPanel (2, 2) {
		contents += new Button {
			action = Action ("Recommencer une partie") {
	    		Ksparov.frame.contents = new DrawNotYet.NotYet
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

	class Border extends GridPanel (Constants.nb_case, 1) {
		for (i <- 0 to Constants.nb_case - 1) {
			contents += new BorderCase(i, 1)
		}
	}

	class Board extends BorderPanel {
		layout(new Header) = North
		layout(new Border) = West
		layout(new Border) = East
		layout(new Grid) = Center
	}
}

/*Objet comprenant les méthodes pour dessiner des actions : mouvement des pièces, etc !*/
object DrawActions {
	var coord = 0
	var player_path = ""
	var piece_path = ""

	def draw_game_board (game_board : Array[Piece]) {
		for (i <- 0 to game_board.length - 1) {
			coord = (game_board(i).pos_x - 1) * 8 + (game_board(i).pos_y - 1)
			if (game_board(i).player == 1) {
				player_path = "1/"
			} else {
				player_path = "2/"
			}
			game_board(i).piece_name match {
        		case "autre" => piece_path = ""
				case "pawn" => piece_path = "Pawn.png"
				case "queen" => piece_path = "Queen.png"
				case "king" => piece_path = "King.png"
				case "rook" => piece_path = "Rook.png"
				case "knight" => piece_path = "Knight.png"
				case "bishop" => piece_path = "Bishop.png"
			}
			DrawBoard.grid_cases(coord).icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player_path + piece_path)
		}
	}
}
