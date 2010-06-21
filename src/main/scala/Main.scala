import java.nio.channels._
import java.nio.charset._
import java.nio._
import scalaz._
import http.request.{Uri, Methods, Line, Request}
import http.Version
import blockster.http.{BodgyRequest, Http}
import Scalaz._
import Iteratee._
import blockster._
import Iteratees._

object Main {
  def main(args: Array[String]) {
    println("Starting")
    val charset: Charset = Charset.forName("US-ASCII")

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
