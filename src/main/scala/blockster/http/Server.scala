package blockster
package http

import scalaz._
import Scalaz._

import java.nio.{CharBuffer, ByteBuffer}
import scalaz.http.response.Response
import scalaz.http.request.{Line, Request}
import java.nio.charset.Charset

case class Server(port: Int = 80, app: BodgyRequest => ByteBuffer) {
  val charset = Charset.forName("US-ASCII")

  def badRequest = {
    val encoder = charset.newEncoder
    encoder.encode(CharBuffer.wrap("Bad request.\n"))
  }

  def run {
    val s = NioServer(port) {
      for (bodgy <- Http.parseRequest) yield {
        (bodgy ∘ app) | badRequest
      }
    }

    try {
      while (true) {
        s.step_!
      }
    } finally {
      s.close_!
    }
  }
}