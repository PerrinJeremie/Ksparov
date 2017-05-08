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

	def main(args: Array[String]) {

	val command = "/usr/games/gnuchessx"
//	val command = "/home/simon/Scripts/pipe.sh"

	val proc = Runtime.getRuntime.exec(command)
	val lineList = List("b2b3\n", "a2a3\n", "c2c3\n", "d2d3\n", "e2e3\n")

	// redirect proc stderr to System.err
	new Thread("stderr reader for " + command) {
	  override def run() {
		for(line <- Source.fromInputStream(proc.getErrorStream).getLines)
		  System.err.println(line)
	  }
	}.start()

	var thread_in_life = true
	var something_to_send = false

	var i = 0

	//pipe data to proc stdin
	new Thread {
		override def run() {
			var out = new PrintWriter(proc.getOutputStream)
			while (thread_in_life) {
				if (something_to_send) {
					out.write(lineList(i))
					out.flush()
					something_to_send = false
					i += 1
				} else {
					Thread.sleep(300)
				}
			}
			out.close()
		}
	}.start()

	var curr_char = 'a'
	var curr_line = ""
	var curr_move = 0

	val move = new Regex("""[0-9]+[.](.*)\n""")
	val opp_move = new Regex("""My move is : (.*)\n""")

	new Thread {
		override def run {
			while (thread_in_life) {
				Thread.sleep(100)
				curr_char = proc.getInputStream.read().toChar
				curr_line += curr_char
				if (curr_char == '\n') {
					curr_line match {
						case "Chess\n" => print(curr_line)
							something_to_send = true
						case move(s) => print(curr_line)
						case opp_move(s) => print(s)
							something_to_send = true
						case _ => print(curr_line)
					}
					curr_line = ""
					curr_char = 'a'
				}
			}
		}
	}.start()

}
}