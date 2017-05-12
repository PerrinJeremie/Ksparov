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
	//pipe data to proc stdin
	class SendThread extends Thread {
		override def run() {
			var out = new PrintWriter(Ksparov.curr_game.gnuchess.getOutputStream)
			while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
				if (Ksparov.curr_game.something_to_send && Ksparov.curr_game.ready_to_gnu) {
					out.write(Ksparov.curr_game.write_to_the_pipe)
					out.flush()
					Ksparov.curr_game.something_to_send = false
				} else {
					Thread.sleep(300)
				}
			}
			out.close()
		}
	}

	var curr_char = 'a'
	var curr_line = ""
	val opp_move = new Regex("""My move is : (.*)\n""")
	val promotion = new Regex("""My move is : (.*)([q|r|b|n])\n""")

	class ListenThread extends Thread {
		override def run {
			while (Ksparov.curr_game.thread_in_life && !Ksparov.curr_game.game_nulle && !Ksparov.curr_game.game_won) {
				Thread.sleep(100)
				curr_char = Ksparov.curr_game.gnuchess.getInputStream.read().toChar
				curr_line += curr_char
				if (curr_char == '\n') {
					curr_line match {
						case "Chess\n" => print(curr_line)
							Ksparov.curr_game.ready_to_gnu = true
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
		}
	}

	class PipePlayer(id : Int) extends Player (id) {
		override def check_pat : Boolean = {
			false
		}

		override def ai_promotion : Unit = {
			()
		}

		def get_piece = {
			var gnu_move = Ksparov.curr_game.last_move_gnuchess
			Ksparov.get_piece_of_pos (gnu_move(0).toInt - 97, gnu_move(1).asDigit - 1, 0)
		}

		override def getmove : Unit = {
			if (Ksparov.curr_game.new_move_available) {
				get_piece
				var gnu_move = Ksparov.curr_game.last_move_gnuchess
				Ksparov.curr_game.board(Ksparov.curr_game.selected_piece).move(gnu_move(2).toInt - 97, gnu_move(3).asDigit - 1, Ksparov.curr_game.board)
				DrawActions.draw_game_board(Ksparov.curr_game.board)
				Ksparov.curr_game.players(Ksparov.curr_game.curr_player).moved = true
				Ksparov.curr_game.new_move_available = false
			}
		}
	}
}