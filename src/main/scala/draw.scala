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
import scala.language.postfixOps
import java.awt.image.BufferedImage  
import java.awt.Image                                                                                            
import javax.imageio.ImageIO
import java.awt.{Graphics2D,Color,Font,BasicStroke}       

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
				/* The size of a grid (the same for the entire application) is difined in the object Display
				   in game.scala, idem for paths to resources. */

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

	/** Returns the string passed in argument without the 4 last characters. 
	*
	* @param s The string that should be cut
	* @return The argument minus its four last characters
	*/
    def shorten(s : String) : String = {
        return s.substring(Display.save_path.length, s.length - 4)
    }

    /** Predicates if a string contains at least one space character. 
    *
    * @param s The inquired string 
    * @return True if the string contains a space caracter
    */
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

    /** Defines the button for the loading menu. 
    *
    * @param text Text display on the button
    * @param return_type Defines the action when the button is pressed
    */
  	class Option (text : String, return_type : String) extends PrettyBigButton {
		action = Action (text) {
			return_type match {
				case "Menu" => Ksparov.frame.contents = new DrawMenu.Menu
					Ksparov.frame.peer.setLocationRelativeTo(null)
				case "Game_begin" =>
        	        Ksparov.curr_game = new Ksparov.Game (6, 1, false)
        	        Load.list_of_moves = List()
        	        Load.get_list_move_from_file(scroll.item)
        	        Ksparov.init_game(6)
        	        Ksparov.frame.contents = new DrawBoard.Board
					Ksparov.frame.peer.setLocationRelativeTo(null)
              case "Game_end" =>
                    Ksparov.curr_game = new Ksparov.Game (7, 1,false)
                    Load.list_of_moves = List()
                    Load.get_list_move_from_file(scroll.item)
        	        Ksparov.init_game(7)
                    Ksparov.play_move
                    Ksparov.frame.contents = new DrawBoard.Board
					Ksparov.frame.peer.setLocationRelativeTo(null)
              case "Delete" =>
                    val res_ = ("rm " + Display.save_path + scroll.item + ".pgn") !!;
                    define_listgame
                    Ksparov.frame.contents = new DrawCharge.Dcharge
			}
		}
	}

	/** Defines the labels used in the loading menu, with the characteristics defined in Display. 
	*
	* @param text The text of the label
	*/
	class PrettyLabel (text : String) extends PrettyBigLabel (text) {
		background = new Color (200, 200, 200)
		opaque = true
	}

	/** The main grid of the loading menu with everything. */
	class CenterGrid extends GridPanel (12,1) {
		for (i <- 0 to 11) {
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
                    	contents += new Option ("<html><div style='text-align : center;'>Charger la partie depuis le début</html>", "Game_begin")
                    } else {
                    	contents += new PrettyLabel ("<html><div style='text-align : center;'> Chessgames.com pour télécharger des parties !</html>")
                    }
                case 8 =>
                    if (!list_empty) {
                    	contents += new Option ("<html><div style='text-align : center;'>Charger la partie à la fin</html>", "Game_end")
                    } else {
                    	contents += new BackgroundCase (1,5)
                    }
		  		case 10 => contents += new Option ("<html><div style='text-align : center;'>Revenir au menu</html>", "Menu")
		  		case _ => contents += new BackgroundCase (1, 5)
    		}
		}
	}

	/** The final menu with the central grid and background columns on each sides of it. */
	class Dcharge extends BorderPanel {
		define_listgame
    	layout (new BackgroundCase (12,1)) = East
    	layout (new BackgroundCase (12,1)) = West
    	layout (new CenterGrid) = Center
	}
}

/** Draws the menus used to save games.*/
object DrawSave {

    /** A text field that stores a certain information conserning the save, to be filled by the user. 
    *
    * @param default The default string display in the textfield
    * @param col The columns0 argument of a TextField*/
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

    /** The label that indicates what the texfield bellow stands for
    *
    * @param str Text display on the label
    */
  	class SaveLabel (str : String) extends PrettyBigLabel (str) {
		preferredSize = Display.dim_message_drawer
		minimumSize = Display.dim_message_drawer
		maximumSize = Display.dim_message_drawer
		background = new Color (200, 200, 200)
		opaque = true
  	}

    /** Returns the result tag associated with (player, game_won, game_null). 
    *
    * @param p Player id 
    * @param gw True if the game is won
    * @param gn True if the game is nulle 
    * @return The result tag for the game
    */
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

    /** Button component which saves game and brings you back where the retur_type tells you to
    *
    * @param text The text display on the button
    * @param return_type Where you want to come back on the click of the button, defines the action of the button
    */
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

    /** Button switches between simple-save (only choice is filename), and advanced-save (all choices). 
    *
    * @param switch_type Defines which type of save you want to go to
    */
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

    /** A panel which gather all components of the advanced-menu */
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
	class Header extends GridPanel (2, 2) {
		// Start a new game button
		contents += new Button {
			font = Display.text_font
			action = Action ("Recommencer une partie") {
				// Stop the threads and wait for them to finish
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				// Draw a game selection menu of the same kind of game as the actual one
	    		Ksparov.frame.contents = new DrawGameSelection.Menu (Ksparov.curr_game.alice_chess)
				Ksparov.frame.peer.setLocationRelativeTo(null)
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
				// Draw the main menu 
				Ksparov.frame.contents = new DrawMenu.Menu
				Ksparov.frame.peer.setLocationRelativeTo(null)
			}
		}
		// Quit Ksparov button
		contents += new Button {
			font = Display.text_font
			action = Action ("Quitter Ksparov") {
				// Stop the threads and wait for them to finish
				Ksparov.curr_game.thread_in_life = false
				Ksparov.curr_game.ai_move.join
				Ksparov.curr_game.timer.join
				// Close the frame so quit Ksparov 
	    		Ksparov.frame.dispose()
			}
		}
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
				    Ksparov.curr_game.players (1 - player_id) = new AI (1 - player_id)
                } else {
                    // Creates new players for Human vs Human
                    Ksparov.curr_game.game_type = 1
                    Ksparov.curr_game.players (player_id) = new Human (player_id)
                    Ksparov.curr_game.players (1 - player_id) = new Human (1 - player_id)

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
                case 2 => 
                    // Displays if needed
                    contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                            Ksparov.curr_game.play_buttons (3)
                        } else { 
                            new BackgroundCase (1, 1)
                        })
                    contents += new BackgroundCase (1, 1)
                    contents += new BackgroundCase (1, 1)
				case 3 =>
                    // Displays if needed
					contents += (if (Array(6,7).contains(Ksparov.curr_game.game_type)) {
                            Ksparov.curr_game.play_buttons (2)
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
				if (i > 7) {
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
		for (i <- 0 to game_board.length - 1) {
			/** Coordinates in one dimension. */
			var coord = game_board(i).pos_x + game_board(i).pos_y * 8
			// If the piece is alive, update the icon of the case of its position.
			if (coord >= 0) {
				Ksparov.curr_game.grids(game_board(i).grid)(coord).action.icon = new javax.swing.ImageIcon(Display.resources_path + Display.pieces_path + game_board(i).player.toString + "/" + game_board(i).piece_path)
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
		}
	}
}
