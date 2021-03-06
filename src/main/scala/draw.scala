import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.io.Source
import java.io.PrintWriter
import scala.util.matching.Regex
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import sys.process._
import scala.language.postfixOps
import java.awt.image.BufferedImage  
import java.awt.Image                                                                                            
import javax.imageio.ImageIO
import java.awt.{Graphics2D,Color,Font,BasicStroke} 
import javax.swing.JFileChooser     
import java.io.{File,FileInputStream,FileOutputStream}

/* This file is organised in objects, each of then draw a certain windows.
   To change the application window, we juste change the contents of Ksparov.frame in game.scala. */

/* The exception of the above rule : */

/** This class loads an image on a stretchable component */
class ImagePanel extends Panel {
  
  /** Stores the path of the image's file */
  var _imagePath = "" 
  
  /** Stores the bufferedImage */
  var bufferedImage:BufferedImage = null                              
  
  /** Function to retrieve path */
  def imagePath = _imagePath                                                  
  
  /** Function to write the new path to the imagePath value and load image
    * @param value the path given as a string 
   */
  def imagePath_=(value:String)                                               
  {                                                                           
    _imagePath = value                                                        
    bufferedImage = ImageIO.read(new File(_imagePath))                        
  }                                                                           

  /** A modified version of paintComponent which takes into account the scaling depending on the size of the window
    *  @param g a Graphics instance (context in which to draw)
    */
  override def paintComponent (g : Graphics2D) = {                                                                           
    if (null != bufferedImage){
      g.drawImage(bufferedImage.getScaledInstance(this.size.width, this.size.height, java.awt.Image.SCALE_SMOOTH), 0, 0, null)
      g.dispose()
    }
  }

} 

/**  This class loads an image on a stretchable component with a label on it
  *  @param f the font to use
  *  @param s the string to print on theiImage  
*/
class ImagePanelWithText(f: Font,s :String) extends ImagePanel
{

  /** Function to write the new path to the imagePath value, loads image and writes over it 
    * @param value the path given as a string
    */
  override def imagePath_=(value:String)                                               
  {                                                                           
    _imagePath = value                                                        
    bufferedImage = ImageIO.read(new File(_imagePath))
    val canvas = bufferedImage.createGraphics()
    canvas.setColor(Color.white)
    canvas.setFont(f)
    canvas.drawString(s, bufferedImage.getWidth/2-8, bufferedImage.getHeight/2+8)
    canvas.dispose()
  }       

  /** A modified version of paintComponent which takes into account the scaling depending on the size of the window
    * @param g a Graphics instance (context in which to draw) 
    */
  override def paintComponent(g:Graphics2D) =                                 
  {                                                                           
    if (null != bufferedImage){
      g.drawImage(bufferedImage.getScaledInstance(this.size.width, this.size.height, java.awt.Image.SCALE_SMOOTH), 0, 0, null)
      g.dispose()
    }
  }
}


/** This class defines a grid of background cases, each background is juste a set of this grid.
*
* @param i Height of the grid
* @param j Width of the grid
*/
class BackgroundCase (i : Int, j : Int) extends GridPanel (i, j) {
	for (k <- 0 to i - 1) {
		for(l <- 0 to j - 1) {
			contents += new ImagePanel {
              imagePath = (Display.resources_path + Display.texture_path)
              	preferredSize = Display.dim_small
				minimumSize = Display.dim_small
				maximumSize = Display.dim_small
            }
        }
	}
}

/** The label we will use with preferences already set *
*
* @param text Text of the label
*/
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
	the role of a particular is defined depending on the parameter passed to the class. 
	*
	* @param name The name on the button, defines the behaviour of the button 
	*/
	class Option (name : String) extends PrettyBigButton {
		action = Action (name) {
			name match {
				case "<html><div style='text-align : center;'>Jouer une<br>partie classique</html>" =>
					Ksparov.frame.contents = new DrawGameSelection.Menu (false)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "<html><div style='text-align : center;'>Jouer aux<br>échecs d'Alice</html>" =>
					Ksparov.frame.contents = new DrawGameSelection.Menu (true)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "<html><div style='text-align : center;'>Jouer une partie<br>avec Gnuchess</html>" => Ksparov.frame.contents = new DrawMenu.Menu
					Ksparov.frame.contents = new DrawGameSelectionGnuChess.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Charger une partie" =>
                    DrawCharge.define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Gérer les paramètres" => 
					Ksparov.frame.contents = new DrawParameters.SubMenus (1)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Quitter Ksparov" => Ksparov.frame.dispose()
			}
		}
	}

	/** The principal menu : for each line there are two buttons and a background between them. */
	class MenuGrid extends GridPanel (7,3) {
    	for( i <- 0 to 6) {
    		i match {
	        	case 1 =>
                    contents += new Option ("<html><div style='text-align : center;'>Jouer une<br>partie classique</html>")
					contents += new BackgroundCase (1, 3)
        			contents += new Option ("<html><div style='text-align : center;'>Jouer aux<br>échecs d'Alice</html>")
                    
        		case 3 =>
        			contents += new Option ("<html><div style='text-align : center;'>Jouer une partie<br>avec Gnuchess</html>")
        			contents += new BackgroundCase (1, 3)
        			contents += new Option ("Charger une partie")
                    
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
  class Menu extends BoxPanel(Orientation.Horizontal) {
    contents += new BorderPanel {
      layout(new BackgroundCase (7, 1)) = West
      layout(new MenuGrid) = Center
      layout(new BackgroundCase (7, 1)) = East
	}
  }
}

/** Object to draw the menu for the game selection : Human vs Human, AI vs AI or AI vs Human. */
object DrawGameSelection {

	/** Label to remember the player what type of game he has chosen : Alice or classic. 
	*
	* @param alice True if the game will be an Alice one, defines the text drawn
	*/
	class MessageDrawer (alice : Boolean) extends Label {
		font = Display.text_font
		background = new Color (200, 200, 200)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		opaque = true
		// Draw the message corresponding to the mode of play chosen between Alice and classic. 
		if (alice) {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs d'Alice,<br>veuillez sélectionner le type de jeu !</html>"
		} else {
			text = "<html><div style='text-align : center;'>Vous avez choisi de jouer aux échecs classiques,<br>veuillez sélectionner le type de jeu !</html>"
		}
	}

	/** Button for lauching the game, it creates a new game with the right parameters depending on the mode of play 
	*
	* @param name The text display by the button
	* @param num The id of the game_type if this button is chosen
	* @param alice True if the game will be an Alice one
	*/
	class Option (name : String, num : Int, alice : Boolean) extends PrettyBigButton {
		action = Action (name) {
			// Create the new game with its parameters
			if (alice) {
				Ksparov.curr_game = new Ksparov.Game (num, Parameters.nb_alice_board, alice)

			} else {
				Ksparov.curr_game = new Ksparov.Game (num, 1, alice)
			}
			// Initialize the game
		    Ksparov.init_game (num)
    		Ksparov.frame.contents = new DrawBoard.Board
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	/** Menu for the type of game selection. 
	*
	* @param alice True if the game will be an Alice one
	*/
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

	/** Draw the welcome message with bordercases around. 
	*
	* @param alice True if the game will be an Alice one 
	*/
	class WelcomeMessage (alice : Boolean) extends BorderPanel {
		layout (new BackgroundCase (1, 11)) = North
		layout (new BorderPanel {
			layout (new BackgroundCase (1, 1)) = West
			layout (new MessageDrawer (alice)) = Center
			layout (new BackgroundCase (1, 1)) = East
		}) = South

	}

	/** Final menu with buttons on. 
	*
	* @param alice True if the game will be an Alice one 
	*/
	class Menu (alice : Boolean) extends BorderPanel {
		layout (new BackgroundCase (7, 1)) = East
		layout (new CenterGrid (alice)) = Center
		layout (new BackgroundCase (7, 1)) = West
		layout (new WelcomeMessage (alice)) = North
	}
}

object DrawGameSelectionGnuChess {
	/** Title
	*/
	class MessageDrawer extends Label {
		font = Display.text_font
		background = new Color (200, 200, 200)
		border = new javax.swing.border.LineBorder (Color.black, 2)
		opaque = true
		text = "<html><div style='text-align : center;'>Vous avez choisi de jouer avec Gnuchess,<br>veuillez sélectionner le type de jeu !</html>"
	}

	/** Button for lauching the game, it creates a new game with the right parameters depending on the mode of play 
	*
	* @param name The text display by the button
	* @param num The id of the game_type if this button is chosen
	*/
	class Option (name : String, num : Int) extends PrettyBigButton {
		action = Action (name) {
			Ksparov.curr_game = new Ksparov.Game (num, 1, false)
			// Initialize the game
		    Ksparov.init_game (num)
    		Ksparov.frame.contents = new DrawBoard.Board
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	}

	/** Menu for the type of game selection. 
	*/
	class CenterGrid extends GridPanel (7, 3) {
		for (i <- 0 to 6) {
			i match {
				case 1 =>
					contents += new Option ("<html><div style='text-align : center;'>Gnuchess blanc vs <br>IA noire</html>", 8)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("<html><div style='text-align : center;'>Gnuchess noir vs <br>IA blanche</html>", 9)
				case 3 =>
					contents += new Option ("<html><div style='text-align : center;'>Gnuchess blanc vs <br>Humain noir</html>", 10)
					contents += new BackgroundCase (1, 3)
					contents += new Option ("<html><div style='text-align : center;'>Gnuchess noir vs <br>Humain blanc</html>", 11)
				case 5 =>
					contents += new BackgroundCase (1, 3)
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

	/** Draw the welcome message with bordercases around. 
	*/
	class WelcomeMessage extends BorderPanel {
		layout (new BackgroundCase (1, 11)) = North
		layout (new BorderPanel {
			layout (new BackgroundCase (1, 1)) = West
			layout (new MessageDrawer) = Center
			layout (new BackgroundCase (1, 1)) = East
		}) = South

	}

	/** Final menu with buttons on. 
	*/
	class Menu extends BorderPanel {
		layout (new BackgroundCase (7, 1)) = East
		layout (new CenterGrid) = Center
		layout (new BackgroundCase (7, 1)) = West
		layout (new WelcomeMessage) = North
	}
}

/** Draw the chess board of the game */
object DrawBoard {

	/** Draw a clock for the player given in argument. 
	*
	* @param player The player the clock is for
	*/
	class Clock (player : Int) extends Label {
		// The color depends on the player black background and white text for player 0 and the opposite for the player 1 
		background = new Color (player * 255, player * 255, player * 255)
		// We should define the opaque to true because by default a label is not opaque
		opaque = true
	    border = new javax.swing.border.LineBorder (Color.black, 2)
		preferredSize = Display.dim_big
		minimumSize = Display.dim_big
		maximumSize = Display.dim_big
		// We use the font defines in Display to adjust it to the resolution of the screen
		font = Display.text_font
		// We display the current time of the player
        /** Called to change the text in this label, because the time has changed */
        def change_time : Unit = {
            var actual_time = Time.int_to_hhmmss(Ksparov.curr_game.players(player).actual_time)
            var nb_move = math.max(Time.periods(Ksparov.curr_game.players(player).actual_period).nb_move - Ksparov.curr_game.players(player).nb_move, 0)
            var inc = Time.periods(Ksparov.curr_game.players(player).actual_period).inc
            var inc_string = 
                if (inc == 0) {
                     ""
                } else {
                    "<br>Incrément : + " + inc.toString
                }
            if (Ksparov.curr_game.players(player).actual_time <= 5 && nb_move > 0) {
                foreground = Color.red
            } else { 
                foreground = new Color ((1 - player) * 255, (1 - player) * 255, (1 - player) * 255)
            }
            if (Ksparov.curr_game.players(player).actual_period + 1 == Time.periods.length) {
                text = "<html><div style='text-align : center;'>" + "Dernière période <br>" + actual_time  + "<br>" + inc_string + "</html>"
            } else {
                text = "<html><div style='text-align : center;'>" + "Encore " + nb_move + " coups <br>" + actual_time + inc_string + "</html>"
            }
        }
        //tries to change time but if there are initializing errors, it's no problem. It just doesn't do it.
        try {
            change_time
        }
        catch {
            case _ : Throwable => ()
        }
	}

	/** Defines backgroundcase with the given string on it 
	*
	* @param label The text display by the label : the reference of a lign or column (A, B, C ..., 1, 2, 3 ...)
	*/
	class BackgroundCaseWithLabel (label : String) extends ImagePanelWithText(Display.text_font,label) {
		preferredSize = Display.dim_small
		imagePath = (Display.resources_path +Display.texture_path)
	}

	/** Cases to represent the dead piece, are also used as promotion button 
	*
	* @param player The player the dead case is for
	* @param piece The piece the icon will be for
	*/
	class DeadCase (player : Int, piece : String) extends Button {
		preferredSize = Display.dim_small
		background = new Color (121, 128, 129)
		action = new Action ("") {
			// The icon is a piece image, it is the same wether the button is enabled or not
			icon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + player.toString + "/" + piece + ".png")
			disabledIcon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + player.toString + "/" + piece + ".png")
			border = new javax.swing.border.LineBorder (Color.black, 1)
			enabled = false
			// If there is a promotion ongoing the button will be enabled and the action defines the piece for the promotion
			def apply = {
				Ksparov.curr_game.selected_promotion = piece
				Ksparov.promotion (Ksparov.curr_game.curr_player)
			}
		}
	}

	/** Cases which represent how many piece of a certain type are dead 
	*
	* @param player The player the number of dead piece stand for
	* @param number The number of dead piece that will be displayed
	*/
	class NumDeadCase (player : Int, number : Int) extends ImagePanelWithText(Display.num_dead_font,number.toString) {
		preferredSize = Display.dim_small
		imagePath = (Display.resources_path + Display.texture_path)
	}

	/** Basic case of the board, colored in black or white and an action that defines the selected piece for mouvment 
	*
	* @param x The x coordinate of the case
	* @param y The y coordinate of the case
	* @param grid_id The id of the grid on which the case is
	*/
	class Case (x : Int, y : Int, grid_id : Int) extends Button {
		preferredSize = Display.dim_small
		if ((x + y) % 2 == 0) {
			background = Color.black
		} else {
			background = Color.white
  		}
  		action = new Action ("") {
			def apply = {
				// The click on a case gives the case selected and the grid selected for moving
				Ksparov.curr_game.selected_case = x + y * 8
				Ksparov.curr_game.selected_grid = grid_id
                Ksparov.play_move
			}
		}
	}

	/** Method which initializes the array of every grids of the game and fill it with each grid */
	def init_grids {
		Ksparov.curr_game.grids = new Array [Array[DrawBoard.Case]] (Ksparov.curr_game.nb_grid)
		for (i <- 0 to Ksparov.curr_game.nb_grid - 1) {
			Ksparov.curr_game.grids (i) = new Array [Case] (Parameters.nb_case_board * Parameters.nb_case_board)
		}
	}

	/** Fill the array of grids with cases of each grid */
	def create_grid_cases {
		for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
			for (i <- 0 to Parameters.nb_case_board - 1) {
				for (j <- 0 to Parameters.nb_case_board - 1) {
					Ksparov.curr_game.grids (k) (i + j * 8) = new Case (i, j, k)
				}
			}
		}
	}

	/** Initializes the set of dead cases for the game */
	def create_grid_dead {
		for (j <- 0 to 1) {
			Ksparov.curr_game.promotion_buttons(j)(0) = new DeadCase (j, "Queen")
			Ksparov.curr_game.promotion_buttons(j)(1) = new DeadCase (j, "Bishop")
			Ksparov.curr_game.promotion_buttons(j)(2) = new DeadCase (j, "Knight")
			Ksparov.curr_game.promotion_buttons(j)(3) = new DeadCase (j, "Rook")
		}
	}

	/** Center element of the frame with the board, labels around but not on the right side of the board 
	*
	* @param grid_id The id of the grid 
	*/
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
						// We use 7 - i here because we want "classic" axis from left to right and from bottom to top. 
						contents += Ksparov.curr_game.grids (grid_id) (j + (7 - i) * 8)
					}
				}
			}
		}
	}

	/** Creates of column with background cases and the number of the case on it */
	class Number_column extends GridPanel (Parameters.nb_case_board + 2, 1) {
		for (i <- 0 to Parameters.nb_case_board + 1) {
			if (i < 1 || i > 8) {
				contents += new BackgroundCase (1, 1)
			} else {
				contents += new BackgroundCaseWithLabel ((9 - i).toString)
			}
		}
	}

	/** The GridPanel with every grid of the game, useful for drawing every amount of grids. */
	class Grid extends GridPanel (1, Ksparov.curr_game.nb_grid) {
		for (k <- 0 to Ksparov.curr_game.nb_grid - 1) {
			contents += new Simple_Grid (k)
		}
	}

	/** Defines a little menu to save, start a new game, come back to the main menu and quit Ksparov */

	class Header1 extends GridPanel (2, 2) {
		// Start a new game button
		contents += new Button {
			font = Display.text_font
			action = Action ("Recommencer une partie") {
				// Stop the threads and wait for them to finish
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				Ksparov.curr_game.write_to_the_pipe = "exit\n"
				print("RECOMMENCER \n")
				Ksparov.curr_game.something_to_send = true
				Ksparov.curr_game.send_to_gnuchess.join
				Ksparov.curr_game.listen_to_gnuchess.join
				if (Ksparov.curr_game.game_type > 7) {
		    		Ksparov.frame.contents = new DrawGameSelectionGnuChess.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				} else {
					// Draw a game selection menu of the same kind of game as the actual one
		    		Ksparov.frame.contents = new DrawGameSelection.Menu (Ksparov.curr_game.alice_chess)
					Ksparov.frame.peer.setLocationRelativeTo(null)
				}
			}
		}
		// Save button
		contents += new Button {
			font = Display.text_font
			action = Action ("Sauvegarder la partie") {
				// Stop the threads and wait for them to finish
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				// Draw the save menu
				Ksparov.frame.contents = new DrawSave.SimpleSave
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		// Come back to main menu button
		contents += new Button {
			font = Display.text_font
			action = Action ("Revenir au menu principal") {
				// Stop the threads and wait for them to finish
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join				
                Ksparov.curr_game.timer.join
				Ksparov.curr_game.write_to_the_pipe = "exit\n"
				print("REVENIR AU MENU PRINCIPAL \n")
				Ksparov.curr_game.something_to_send = true
				Ksparov.curr_game.send_to_gnuchess.join
				Ksparov.curr_game.listen_to_gnuchess.join
				Ksparov.curr_game = null
				// Draw the main menu 
				Ksparov.frame.contents = new DrawMenu.Menu
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
	  // Change parameters
	  contents += new Button {
		font = Display.text_font
		action = Action ("Gérer les paramètres") {
			// Stop the threads and wait for them to finish
			Ksparov.curr_game.thread_in_life = false
			Ksparov.curr_game.ai_move.join
			Ksparov.curr_game.timer.join
			// Close the frame so quit Ksparov
    	    DrawParameters.where_cb = 1
	        Ksparov.frame.contents = new DrawParameters.SubMenus (2)
			Ksparov.frame.peer.setLocationRelativeTo(null)
		}
	  }
	}

  class Header extends GridBagPanel {

    def constraints(x: Int, y: Int, 
		    gridwidth: Int = 1, gridheight: Int = 1,
		    weightx: Double = 0.0, weighty: Double = 0.0,
		    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.Both) 
    : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }

    add(new Header1, constraints(0,0))
    add(new Button {
      font = Display.text_font
	  action = Action ("<html><div style='text-align : center;'> Quitter <br> Ksparov </html>") {
		// Stop the threads and wait for them to finish
		Ksparov.curr_game.thread_in_life = false
		Ksparov.curr_game.write_to_the_pipe = "exit\n"
		print("QUITTER \n")
		Ksparov.curr_game.something_to_send = true
		Ksparov.curr_game.ai_move.join
		Ksparov.curr_game.timer.join
		Ksparov.curr_game.send_to_gnuchess.join
		Ksparov.curr_game.listen_to_gnuchess.join
		// Close the frame so quit Ksparov
	    Ksparov.frame.dispose()
	  }
    }, constraints(12,0,gridwidth = 10))
  }

	/** Defines the label which display game messages passed in argument : mat, pat ... 
	*
	* @param message The message displayed by the label
 	*/
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

	/** Defines the footer of the board screen with the message drawer on it */
	class Footer extends BorderPanel {
        /** The clock for the white player. */
        var clock1 = new Clock (1)
        /** the clock for the black player. */
        var clock0 = new Clock (0)
        Ksparov.curr_game.clock_array = Array(clock0,clock1)
		layout(new BackgroundCase (1, 2)) = East
		layout(new BackgroundCase (1, 2)) = West
		layout(new BorderPanel {
			// We only draw a clock if clocks are enabled 
			layout (if (Time.clock_available) {
					clock1
				} else {
					new BackgroundCase (1, 3)
				}) = West

			layout (Ksparov.curr_game.message_drawer) = Center

			// We only draw a clock if clocks are enabled 
			layout (if (Time.clock_available) {
					clock0
				} else {
					new BackgroundCase (1, 3)
				}) = East
		}) = Center
	}

	/** Button to start to play in a loaded game, display play button if we are in a loaded game
	*
	* @param player The player that the human will control by clicking on this button
	*/
	class PlayButton (player_id : Int, player_type : String) extends Button {
		preferredSize = Display.dim_small
        listenTo(mouse.moves)
        reactions += {
            case _ : event.MouseEntered  => 
                if (player_type == "ai") {
                    DrawActions.draw_game_messages ("Info PlayButton AI", player_id)
                } else {
                	if (player_type == "h") {
	                    DrawActions.draw_game_messages ("Info PlayButton h", player_id)
	                } else {
	                    DrawActions.draw_game_messages ("Info PlayButton gnu", player_id)
	                }
                }
            case _ : event.MouseExited => DrawActions.draw_game_messages ("Current_turn", Ksparov.curr_game.curr_player)
        }
		action = new Action ("") {
			// If we are in a loaded game, the button is enaled and displays a play button of the color of the player
				enabled = true
				background = Color.red
				borderPainted = true
				border = new javax.swing.border.LineBorder (Color.black, 1)
				icon = new javax.swing.ImageIcon(Display.resources_path + "Play_buttons_" + player_type.toString + player_id.toString + ".png")
			/* When actionned, this button changes players to human players of the selected color and AI for the other, it also
			   disables the play button */
			def apply {
                if (player_type == "ai") {
                    // Creates new players for AI vs Human
    				Ksparov.curr_game.game_type = 2
                    Ksparov.curr_game.players (player_id) = new Human (player_id)
				    Ksparov.curr_game.players (1 - player_id) = new AI2 (1 - player_id)
                } else {
                	if (player_type == "gnu") {
                		Ksparov.curr_game.game_type = 10
                		Ksparov.curr_game.players (player_id) = new Human (player_id)
                		Ksparov.curr_game.players (1 - player_id) = new Pipe.PipePlayer (1 - player_id)
						Ksparov.curr_game.gnuthread_in_life = true
						Ksparov.curr_game.gnuchess = Runtime.getRuntime.exec("gnuchessx -e")
						Ksparov.curr_game.send_to_gnuchess.start()
						Ksparov.curr_game.listen_to_gnuchess.start()
						Ksparov.curr_game.write_to_the_pipe += "setboard " + FEN.board_to_FEN (Ksparov.curr_game.board) + "\n"
						if (player_id != Ksparov.curr_game.curr_player) {
							Ksparov.curr_game.write_to_the_pipe += "go\n"
						} else {
							Ksparov.curr_game.go_to_send = true
						}
						Ksparov.curr_game.something_to_send = true
                	} else {
	    	            // Creates new players for Human vs Human
    	                Ksparov.curr_game.game_type = 1
            	        Ksparov.curr_game.players (player_id) = new Human (player_id)
                	    Ksparov.curr_game.players (1 - player_id) = new Human (1 - player_id)
                	}
                }
                DrawActions.draw_game_messages ("Current_turn", Ksparov.curr_game.curr_player)
				// Disables play button and set their display as a backgroundcase 
				Ksparov.frame.contents = new DrawBoard.Board {
                    preferredSize = Ksparov.frame.contents(0).bounds.getSize()
                }
                if (Ksparov.curr_game.players(Ksparov.curr_game.curr_player).ai) {
                    Ksparov.play_move
                }
			}
		}
	}

	/** Left Border grid with dead pieces for the white player. */
	class Border1 extends GridPanel (Parameters.nb_case_board + 2, 3) {
		for(i <- 0 to Parameters.nb_case_board + 1) {
			i match {
				case 1 => 
                    // Displays if needed
                    contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type) && !Ksparov.curr_game.alice_chess) {
                            Ksparov.curr_game.play_buttons (5)
                        } else { 
                            new BackgroundCase (1, 1)
                        })
                    contents += new BackgroundCase (1, 1)
                    contents += new BackgroundCase (1, 1)
                case 2 => 
                    // Displays if needed
                    contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                            Ksparov.curr_game.play_buttons (4)
                        } else { 
                            new BackgroundCase (1, 1)
                        })
                    contents += new BackgroundCase (1, 1)
                    contents += new BackgroundCase (1, 1)
				case 3 =>
                    // Displays if needed
					contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                            Ksparov.curr_game.play_buttons (3)
                        } else { 
                            new BackgroundCase (1, 1)
                        })
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

	/** Right Border grid with dead pieces for the black player, and labels with number of the cases */
	class Border0 extends GridPanel (Parameters.nb_case_board + 2, 4) {
		for(i <- 0 to Parameters.nb_case_board + 1) {
			if (i < 1 || i > 8) {
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
				contents += new BackgroundCase (1, 1)
			} else {
				if (i > 8) {
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
                        // Displays if needed
						contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                                Ksparov.curr_game.play_buttons (0)
                            } else { 
                                new BackgroundCase (1, 1)
                            })
                    case 7 =>
                        contents += new BackgroundCaseWithLabel ((9 - i).toString)
                        contents += new BackgroundCase (1, 1)
                        contents += new BackgroundCase (1, 1)
                        // Displays if needed
                        contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                                Ksparov.curr_game.play_buttons (1)
                            } else { 
                                new BackgroundCase (1, 1)
                            }) 
                    case 8 =>  
                    	contents += new BackgroundCaseWithLabel ((9 - i).toString)
                        contents += new BackgroundCase (1, 1)
                        contents += new BackgroundCase (1, 1)
                        // Displays if needed
                        contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type) && !Ksparov.curr_game.alice_chess) {
                                Ksparov.curr_game.play_buttons (2)
                            } else { 
                                new BackgroundCase (1, 1)
                            }) 
					}
				}
			}
		}
	}

	/** Final board with everything */
	class Board extends BorderPanel {
		layout(new Header) = North
		layout(new Border1) = West
		layout(new Border0) = East
		layout(new Footer) = South
		layout(new Grid) = Center
	}
}

/** Draw actions on the board : move, promotion... */
object DrawActions {

	/** Draw a game board (an array of pieces) on the chessboard. 
	*
	* @param game_board The board to be drawn
	*/
	def draw_game_board (game_board : Array[Piece]) {
		// Reinitializing the dead piece array to avoid mutli-counting.
		Ksparov.curr_game.dead_pieces = Array(new Array[Int](5), new Array[Int](5))
		// Initilizing the array of cases.
		DrawBoard.create_grid_cases
		if (Ksparov.curr_game.last_move_dep > 0 && Ksparov.curr_game.last_move_arr > 0) {
			Ksparov.curr_game.grids(0)(Ksparov.curr_game.last_move_dep).background = Color.green
			Ksparov.curr_game.grids(0)(Ksparov.curr_game.last_move_arr).background = Color.green
		}
		for (i <- 0 to game_board.length - 1) {
			/** Coordinates in one dimension. */
			var coord = game_board(i).pos_x + game_board(i).pos_y * 8
			// If the piece is alive, update the icon of the case of its position.
			if (coord >= 0) {
				Ksparov.curr_game.grids(game_board(i).grid)(coord).action.icon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + game_board(i).player.toString + "/" + game_board(i).piece_path)
				Ksparov.curr_game.grids(game_board(i).grid)(coord).disabledIcon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + game_board(i).player.toString + "/" + game_board(i).piece_path)
			// Else, if the piece is dead, update the array which counts the number of dead piece for each players.
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

	/** Enables dead button for the promotion so the player can selected the piece for the promotion 
	*
	* @param p The player the promotion is for
	*/
	def enable_promotion (p : Int) {
		Ksparov.curr_game.promotion = true
		for (i <- 0 to 3) {
			Ksparov.curr_game.promotion_buttons(p)(i).enabled = true
			Ksparov.curr_game.promotion_buttons(p)(i).background = Color.red
		}
		Ksparov.frame.contents = new DrawBoard.Board{
          preferredSize = Ksparov.frame.contents(0).bounds.getSize()
        }
	}

	/** Disables dead button because the promotion is over 
	*
	* @param p The player the promotion was for
	*/
	def disable_promotion (p : Int) {
		Ksparov.curr_game.promotion = false
		for (i <- 0 to 3) {
			Ksparov.curr_game.promotion_buttons(p)(i).enabled = false
			Ksparov.curr_game.promotion_buttons(p)(i).background = new Color (121, 128, 129)
		}
		// Actualizes the new board and swith to the next player
		draw_game_board (Ksparov.curr_game.board)
		Ksparov.frame.contents = new DrawBoard.Board{
          preferredSize = Ksparov.frame.contents(0).bounds.getSize()
        }
		draw_game_messages ("Current_turn", 1 - Ksparov.curr_game.curr_player)
	}

	/** Colors in red the reachables cases for a piece on the boad of the piece and the next one 
	*
	* @param case_list The list of cases that will be colored
	* @param piece_position_x The x position of the piece that is about to move
	* @param piece_position_y The y position of the piece that is about to move
	* @param grid The grid on which the piece is 
	*/
	def draw_possible_moves (case_list : Array[(Int, Int)], piece_position_x : Int, piece_position_y : Int, grid : Int) {
		// Coloring the position of the piece yellow on the boad of the piece and the next one 
		Ksparov.curr_game.grids(grid)(piece_position_x + piece_position_y * 8).background = Color.yellow
		Ksparov.curr_game.grids((grid + 1) % (Ksparov.curr_game.nb_grid))(piece_position_x + piece_position_y * 8).background = Color.yellow
		// For each reachable case, colors it into red on the boad of the piece and the next one 
   		for (i <- 0 to case_list.length - 1) {
            var (j, l) = case_list(i)
            Ksparov.curr_game.grids (grid) (j + 8 * l).background = Color.red
            Ksparov.curr_game.grids ((grid + 1) % (Ksparov.curr_game.nb_grid)) (j + 8 * l).background = Color.red
        }
	}

	/** Recolors the board as "normal" cases : white and black. */
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
		if (Ksparov.curr_game.last_move_dep > 0 && Ksparov.curr_game.last_move_arr > 0) {
			Ksparov.curr_game.grids(0)(Ksparov.curr_game.last_move_dep).background = Color.green
			Ksparov.curr_game.grids(0)(Ksparov.curr_game.last_move_arr).background = Color.green
		}
	}

	/** Draw a given message on the board, the message depends on the argument passed 
	*
	* @param message_type The type of message we should display : mat, pat ...
	* @param player The player the message is for
	*/
	def draw_game_messages (message_type : String, player : Int) {
		/** The name of the player, which is defined in the pgnin the case of a loaded game */
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

			// Draw who's player the turn is.
			case "Current_turn" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "La main est à " + joueur_string +" !"
					Ksparov.curr_game.message_drawer.foreground = Color.black
				case _ => Ksparov.curr_game.message_drawer.text = "La main est au " + joueur_string +" !"
					Ksparov.curr_game.message_drawer.foreground = Color.black
			}

			// Draw if a player is in check.
			case "Check" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = joueur_string + " est en échec !"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "Le " + joueur_string + " est en échec !"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}

			// Draw if a player has won thanks to the clock
			case "Time" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Perte au temps,<br>" + joueur_string + " gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw if a player is mate.
			case "Mate" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>" + (if (player == 0) {Load.infos("White")} else {Load.infos("Black")}) + " gagne la partie !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Echec et mat,<br>le "+ (if(player == 0){"joueur blanc"}else{"joueur noir"}) + " gagne la partie !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}

            // Draw if IA has forfeited 
          case "Forfeit" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Forfait,<br>" + (if(player == 0){"joueur blanc"}else{"joueur noir"}) + " gagne la partie !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw if an AI cannot move (this option has only been implemented for IA).
			case "Pat" => Ksparov.curr_game.game_type match {
				case 6 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>"+ joueur_string +" ne peut plus bouger !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
				case _ => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle,<br>le "+ joueur_string +" ne peut plus bouger !</html>"
					Ksparov.curr_game.message_drawer.foreground = Color.red
			}
            
			// Draw if there is no more checkmate possible 
			case "Nulle" => 
                Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Partie nulle, les joueurs ne peuvent ne peuvent plus mater !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red
			
			// Draw if 50 moves has been made without a pawn move or a piece taken
			case "50coups" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 50 coups sans prise ni mouvement de pion ! </html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw if a single position occured 3 times in the same game
			case "TripleRepetition" => 	Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'> Partie nulle : 3 répétitions de la même position ! </html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw if there is a promotion ongoing
			case "Promotion" => Ksparov.curr_game.curr_player match {
				case 0 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion noir !"
				case 1 => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Selectionnez la promotion <br> du pion blanc !"
			}

			// Draw the result of a loaded game
        	case "1-0" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>" + Load.infos("White") +" gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "0-1" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>"+ Load.infos("Black") +" gagne la partie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

        	case "1/2-1/2" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Pat : la partie est nulle !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw if a loaded game is not finished yet 
        	case "*" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Partie non finie !</html>"
				Ksparov.curr_game.message_drawer.foreground = Color.red

			// Draw special comments that could be present in a pgn
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

            case "Info PlayButton AI" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Cliquer pour reprendre la partie avec <br> ce joueur contre une IA</html>"
                Ksparov.curr_game.message_drawer.foreground = Color.red

            case "Info PlayButton h" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Cliquer pour reprendre la partie avec <br> ce joueur contre un humain</html>"
                Ksparov.curr_game.message_drawer.foreground = Color.red

            case "Info PlayButton gnu" => Ksparov.curr_game.message_drawer.text = "<html><div style='text-align : center;'>Cliquer pour reprendre la partie avec <br> ce joueur contre gnuchess</html>"
                Ksparov.curr_game.message_drawer.foreground = Color.red

		}
	}
}
