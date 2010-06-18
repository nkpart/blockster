import java.nio.channels._
import java.nio.charset._
import java.nio._
import scalaz._
import http.request.{Uri, Methods, Line}
import http.Version
import Scalaz._
import Iteratee._
import blockster._
import blockster.http.Http
import Iteratees._

object Main {
  def main(args: Array[String]) {
    println("Starting")
    val charset: Charset = Charset.forName("US-ASCII")

    def app1(line: Line): ByteBuffer = {
        val encoder = charset.newEncoder
        encoder.encode(CharBuffer.wrap(line.shows + "\n"))
    }

    val server = blockster.http.Server(9000, app1)
    server.run
    println("The end.")
  }
}
