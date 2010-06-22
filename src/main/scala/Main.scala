import java.nio.channels._
import java.nio.charset._
import java.nio._
import scalaz._
import http.request.{Uri, Methods, Line, Request}
import http.Version
import Scalaz._
import IterV._
import blockster._
import Iteratees._

object Main {
  def main(args: Array[String]) {
    println("Starting")
    val charset = Charset.forName("US-ASCII")

    /*
      Parrots the request back to the user, logging it and all headers.

      sample response:
      You requested: GET /ben/hi HTTP/1.1
    */
    def app1(r: Request[Stream]): ByteBuffer = {
      val encoder = charset.newEncoder
      println(r.line.shows)
      println(r.headers map {
        case (header, value) => header.asString + " -> " + value.list.mkString
      })
      encoder.encode(CharBuffer.wrap("You requested: " + r.line.shows + "\n"))
    }

    val server = blockster.http.Server(9001, app1)
    server.run
    println("The end.")
  }
}
