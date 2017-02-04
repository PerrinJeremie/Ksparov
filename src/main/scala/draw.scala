import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._

object Board extends SimpleSwingApplication {
	val dim_case = new Dimension (80, 80)
	val nb_case_border = 1
	val nb_case_board = 8
	var nb_case = nb_case_board + 2 * nb_case_border
	var coord = 0
	var resource_path = "src/main/resources/"
	var piece_path = ""
	var player_path = ""
	var selected_case = 0

	/*Classe de test, la classe finale peut ne pas ressembler à ça ! Mais doit contenir ces infos.*/
	class Piece (name : String, play : Int, x : Int, y : Int) {
		var pos_x = x
		var pos_y = y
		var piece_name = name
		var player = play
	}
	val piece1 = new Piece ("pawn", 1, 3, 4)
	val piece2 = new Piece ("queen", 1, 4, 4)
	val piece3 = new Piece ("king", 2, 6, 5)
	var board_test = Array[Piece] (piece1, piece2, piece3) 

	/*Prend une liste de pièces en entrées et dessine le plateau correspondant*/
	def draw_game_board (game_board : Array[Piece]) {
		for (i <- 0 to game_board.length - 1) {
			coord = (game_board(i).pos_x - 1) * 8 + (game_board(i).pos_y - 1)

			if (game_board(i).player == 1) {
				player_path = "Pieces/White/"
			} else {
				player_path = "Pieces/Red/"
			}

			game_board(i).piece_name match {
				case "pawn" => piece_path = "Pawn.png"
				case "queen" => piece_path = "Queen.png"
				case "king" => piece_path = "King.png"
				case "rook" => piece_path = "Rook.png"
				case "knight" => piece_path = "Knight.png"
				case "bishop" => piece_path = "Bishop.png"
			}

			grid_cases(coord).icon = new javax.swing.ImageIcon(resource_path + player_path + piece_path)
		}
	}

	/*Classe des cases, attention les cases commençent en 0,0 et finisse en 7,7, 
	  pour la suite, j'ai utilisé un seul entier pour référencer les cases, une case
	  de coordonnées (x, y) est à la position 8*x+y*/
	class Case (x : Int, y : Int) extends Button {
		preferredSize = dim_case
		if ((x + y) % 2 == 0) {
				background = Color.black
			}
			else {
				background = Color.white
			}
		
		action = Action ("") {
			selected_case = x * 8 + y
			println("Click case nb : " + selected_case.toString)
		}
	}

	/*Prend une liste de case et les colorie en bleu*/
	def draw_possible_move (cases : Array[Int]) {
		for (i <- 0 to cases.length - 1) {
			grid_cases(cases(i)).background = Color.blue
			println ("Je dois changer de couleur")
		}
	}

	class BorderCase (x : Int, y : Int) extends Label {
		icon = new javax.swing.ImageIcon("src/main/resources/Wood_Texture_mini_2.png")
		preferredSize = dim_case
	}

	var grid_cases = new Array[Case](nb_case_board * nb_case_board)
	for (i <- 0 to nb_case_board - 1) {
		for( j <- 0 to nb_case_board - 1) {
			grid_cases(i*8 + j) = new Case(i, j)	
		}
	}

	def board_grid = new GridPanel (nb_case, nb_case) {
		for (i <- 0 to nb_case - 1) {
			for (j <- 0 to nb_case - 1) {
				if (i < nb_case_border || i > nb_case - nb_case_border - 1 || 
					j < nb_case_border || j > nb_case - nb_case_border - 1) {
					contents += new BorderCase (i - nb_case_border, j - nb_case_border)
				}
				else {
					contents += grid_cases((i - nb_case_border) * 8 + j - nb_case_border)
				}
			}
		} 
	}

	var test_move = Array (1, 2, 3, 4, 5, 54)
	
	draw_possible_move (test_move)
	draw_game_board (board_test)

	def header = new GridPanel (2, 2) {
		contents += new Button {text = "Jouer"}
		contents += new Button {text = "Rejouer une partie"}
		contents += new Button {text = "Quitter"}
		contents += new Button {text = "Mes scores"}
	}

	def left = new GridPanel (nb_case, 1) {
		for (i <- 0 to nb_case - 1) {
			contents += new BorderCase(i, 1)
		}
	}

	def right = new GridPanel (nb_case, 1) {
		for (i <- 0 to nb_case - 1) {
			contents += new BorderCase(i, 1)
		}
	}

	var grid = new BorderPanel {
			layout(header) = North
			layout(board_grid) = Center
			layout(left) = West
			layout(right) = East
		}

	def top = new MainFrame {
		title = "Expert Chess"
		contents = grid
		
	}
}
