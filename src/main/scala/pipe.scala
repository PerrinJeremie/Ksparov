import java.io._
import scala.io.Source
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

/**
 * Pipe implementation in Scala.
 */
object Pipe {
	/** 
	* Thread to send things to the pipe
	*/
	class SendThread extends Thread {
		override def run() {
			/** The output stream of gnuchess */
			var out = new PrintWriter(Ksparov.curr_game.gnuchess.getOutputStream)
			// While threads should be alive
			while (Ksparov.curr_game.gnuthread_in_life) {
				// If there is something to send
				if (Ksparov.curr_game.something_to_send && Ksparov.curr_game.ready_to_gnu) {
					//Write in the pipe
					print("J'envoi  : " + Ksparov.curr_game.write_to_the_pipe + '\n')
					out.write(Ksparov.curr_game.write_to_the_pipe)
					// Flush to finish the writing
					out.flush()
					// If we killed gnuchess, killes threads too
					if (Ksparov.curr_game.write_to_the_pipe == "exit\n") {
						Ksparov.curr_game.gnuthread_in_life = false
					}
					// Reinitializa pipe
					Ksparov.curr_game.something_to_send = false
					Ksparov.curr_game.write_to_the_pipe = ""
				} else {
					Thread.sleep(300)
				}
			}
			// Close the stream
			out.close()
		}
	}

	/** Thread to listen what gnuchess has to say */
	class ListenThread extends Thread {
		/** The current char read*/
		var curr_char = 'a'
		/** The current line read */
		var curr_line = ""
		/** The regexpr for gnuchess moves */
		val opp_move = new Regex("""My move is : (.*)\n""")
		/** The regexpr for gnuchess promotion */
		val promotion = new Regex("""My move is : (.*)([q|r|b|n])\n""")
	
		override def run {
			// While threads should be alive
			while (Ksparov.curr_game.gnuthread_in_life) {
				Thread.sleep(100)
				// Read one char from the stream
				curr_char = Ksparov.curr_game.gnuchess.getInputStream.read().toChar
				// Add the char to the line 
				curr_line += curr_char
				// If we are at the end of the line
				if (curr_char == '\n') {
					print(curr_line + '\n')
					// Match the line
					curr_line match {
						case "Chess\n" => Ksparov.curr_game.ready_to_gnu = true
						case promotion(s,l) => 
							l match {
								case "q" => Ksparov.curr_game.selected_promotion = "Queen"
								case "b" => Ksparov.curr_game.selected_promotion = "Bishop"
								case "n" => Ksparov.curr_game.selected_promotion = "Knight"
								case "r" => Ksparov.curr_game.selected_promotion = "Rook"
							}
							Ksparov.curr_game.new_move_available = true
							Ksparov.curr_game.last_move_gnuchess = s
    			            Ksparov.play_move
						case opp_move(s) => 
							Ksparov.curr_game.new_move_available = true
							Ksparov.curr_game.last_move_gnuchess = s
    			            Ksparov.play_move
						case _ => ()
					}
					curr_line = ""
					curr_char = 'a'
				}
			}
			// Empty and close the pipe
			Ksparov.curr_game.gnuchess.getInputStream.read(new Array [Byte] (1000))
			Ksparov.curr_game.gnuchess.getInputStream.close()
		}
	}

	/** Player that plays what he receives from the pipe */
	class PipePlayer(id : Int) extends Player (id) {
		ai = true
		override def check_pat : Boolean = {
			/** Number of moves possible for a player */
    		var sum = 0
    		for (i <- 0 to Ksparov.curr_game.board.length / 2 - 1) {
				sum = sum + Ksparov.curr_game.board(i + 16 * (1 - id)).possible_moves(Ksparov.curr_game.board).length
    		}
    		// If there is no move possible, the player is in pat
    		if (sum == 0) {
				Ksparov.curr_game.game_nulle = true
				true
    		} else {
				false
    		}
		}

		override def ai_promotion : Unit = {
			Ksparov.promotion(Ksparov.curr_game.curr_player)
		}

		/** Transform the notation from the pipe to notation for the game */
		def get_piece = {
			var gnu_move = Ksparov.curr_game.last_move_gnuchess
			// Store the last move departure case to colored it in green
			Ksparov.curr_game.last_move_dep = gnu_move(0).toInt - 97 + (gnu_move(1).asDigit - 1) * 8
			Ksparov.get_piece_of_pos (gnu_move(0).toInt - 97, gnu_move(1).asDigit - 1, 0)
		}

		override def getmove : Unit = {
			// If a new move is available
			if (Ksparov.curr_game.new_move_available) {
				get_piece
				var gnu_move = Ksparov.curr_game.last_move_gnuchess
				// Play the move
				Ksparov.curr_game.board(Ksparov.curr_game.selected_piece).move(gnu_move(2).toInt - 97, gnu_move(3).asDigit - 1, Ksparov.curr_game.board)
				// Store the last move arrival case to colored it in green
				Ksparov.curr_game.last_move_arr = gnu_move(2).toInt - 97 + (gnu_move(3).asDigit - 1) * 8
				DrawActions.draw_game_board(Ksparov.curr_game.board)
				Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true
				Ksparov.curr_game.new_move_available = false
			}
		}
	}
}