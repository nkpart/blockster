import java.nio.channels._
import java.nio.charset._
import java.nio._
import scalaz._
import http.request.{Uri, Methods, Line}
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

    def app1(r: BodgyRequest): ByteBuffer = {
        val encoder = charset.newEncoder
        println(r.headers match {
           case Some(headerLines) => headerLines map (l => l ∘ (_.toChar) mkString) mkString ";"
           case None => "No headers."
        })
        encoder.encode(CharBuffer.wrap("You requested: " + r.line.shows + "\n"))
    }

    val server = blockster.http.Server(9000, app1)
    server.run
    println("The end.")
  }
}
