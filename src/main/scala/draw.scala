import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._

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

	class MenuGrid extends GridPanel (nb_menu_option + 1, 3) {
    	for( i <- 0 to nb_menu_option) {
    		if (i % 2 == 0) {
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
        		contents += new BackgroundCase (1, 3)
    		} else { i match {
	        case 1 => contents += new Resume_button
			contents += new BackgroundCase (1, 3)
        	contents += new Play_button 
        	case 3 => contents += new Load_button
        	contents += new BackgroundCase (1, 3)
        	contents += new Score_button 
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
				Constants.small_texture_path = "Texture_small_" + number.toString + ".png"
				Constants.big_texture_path = "Texture_big_" + number.toString + ".png"
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
				Constants.pieces_path = "Pieces/" + number.toString + "/"
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
			if (i % 2 == 0) {
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

	class DeadCase (player : Int, piece : String) extends Label {
		preferredSize = Constants.dim_small
		background = new Color (121, 128, 129)
		opaque = true
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.pieces_path + player.toString + "/" + piece + ".png")
		border = new javax.swing.border.LineBorder (Color.black, 1)

	}

	class NumDeadCase (player : Int) extends Label {
		preferredSize = Constants.dim_small
		icon = new javax.swing.ImageIcon(Constants.resources_path + Constants.small_texture_path)
		horizontalTextPosition = Alignment.Center
		foreground = Color.red
		font = new Font("Arial", 0, 40)
		var value = 0
		text = value.toString
	}

	/*Tableau des pièces mortes pour les 2 joueurs : chaque case donne le nombre de pièces mortes 
	sachant que 0 Reine, 1 Fou, 2 Cavalier, 3 Tour et 4 Pion*/
	var dead_pieces = Array(new Array[NumDeadCase](5), new Array[NumDeadCase](5))
	for(i <- 0 to 1) {
		for (j <- 0 to 4) {
			dead_pieces(i)(j) = new NumDeadCase(i)
		}
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

	class Grid extends GridPanel (Constants.nb_case_board + 2, Constants.nb_case_board) {
		for (i <- -1 to Constants.nb_case_board) {
			for (j <- 0 to Constants.nb_case_board - 1) {
				if (i < 0 || i > Constants.nb_case_board - 1) {
					contents += new BackgroundCase (1, 1)
				} else {
					contents += grid_cases(i * 8 + j)
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

	class Border1 (player : Int) extends GridPanel (Constants.nb_case, 2) {
		for(i <- 0 to Constants.nb_case - 1) {
			if (i < 3 || i > 7) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else { i match {
				case 3 => 
					contents += new DeadCase (1, "Queen")
					contents += dead_pieces(1)(0)
				case 4 => 
					contents += new DeadCase (1, "Bishop")
					contents += dead_pieces(1)(1)
				case 5 =>
					contents += new DeadCase (1, "Knight")
					contents += dead_pieces(1)(2)
				case 6 => 
					contents += new DeadCase (1, "Rook")
					contents += dead_pieces(1)(3)
				case 7 => 
					contents += new DeadCase (1, "Pawn")
					contents += dead_pieces(1)(4)
				}
			}
		}
	}

	class Border2 (player : Int) extends GridPanel (Constants.nb_case, 2) {
		for(i <- 0 to Constants.nb_case - 1) {
			if (i < 3 || i > 7) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else { i match {
				case 3 => 
					contents += dead_pieces(0)(0)
					contents += new DeadCase (2, "Queen")
				case 4 => 
					contents += dead_pieces(0)(1)
					contents += new DeadCase (2, "Bishop")
				case 5 =>
					contents += dead_pieces(0)(2)
					contents += new DeadCase (2, "Knight")
				case 6 => 
					contents += dead_pieces(0)(3)
					contents += new DeadCase (2, "Rook")
				case 7 => 
					contents += dead_pieces(0)(4)
					contents += new DeadCase (2, "Pawn")
				}
			}
		}
	}

	class Board extends BorderPanel {
		layout(new Header) = North
		layout(new Border1(1)) = West
		layout(new Border2(2)) = East
		layout(new Grid) = Center
	}
}

/*Objet comprenant les méthodes pour dessiner des actions : mouvement des pièces, etc !*/
object DrawActions {

	def draw_game_board (game_board : Array[Piece]) {
		var coord = 0
		var player_path = ""
		var piece_path = ""
		for (i <- 0 to game_board.length - 1) {
			coord = game_board(i).pos_x * 8 + game_board(i).pos_y
			if (coord >= 0) {
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
			} else {
				game_board(i).piece_name match {
					case "queen" => DrawBoard.dead_pieces(game_board(i).player - 1)(0).value += 1
					case "bishop" => DrawBoard.dead_pieces(game_board(i).player - 1)(1).value += 1
					case "knight" => DrawBoard.dead_pieces(game_board(i).player - 1)(2).value += 1
					case "rook" => DrawBoard.dead_pieces(game_board(i).player - 1)(3).value += 1
					case "pawn" => DrawBoard.dead_pieces(game_board(i).player - 1)(4).value += 1 
				}
			}
		}
	}
}
