import java.net.InetSocketAddress
import java.nio.channels._
import java.nio.charset.{CharsetEncoder, Charset}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Set => JSet}
import java.lang.{Iterable => JIterable}
import scalaz._
import Scalaz._
import Iteratee._
import server.Server

object Main {
  implicit def InputFunctor: Functor[Input] = new Functor[Input] {
    def fmap[A, B](r: Input[A], f: A => B): Input[B] = r(empty = Iteratee.Empty[B], eof = EOF[B], el = a => El(f(a)))
  }

  def upto[E](p: E => Boolean) = {
    def step(es: List[E])(s: Input[E]): Iteratee[E, List[E]] = {
      s(el = e => {
        val next = e :: es
        if (p(e)) {Done(next.reverse, Iteratee.Empty[E])} else {Cont(step(next))}
      },
        empty = Cont(step(es)),
        eof = Done(es.reverse, EOF[E]))
    }
    Cont(step(Nil))
  }

  val CR: Byte = 13
  val LF: Byte = 10

  val line: Iteratee[Byte, Option[List[Byte]]] = for (chars <- upto[Byte](_ == CR); next <- head[Byte]) yield {
    next match {
      case Some(LF) => some(chars)
      case _ => none
    }
  }

  def main(args: Array[String]) {
    println("Starting")
    val charset: Charset = Charset.forName("US-ASCII")
    val thing = line map { (x: Option[List[Byte]]) =>
        val encoder: CharsetEncoder = charset.newEncoder
        val r = (x >>= (characters => {
          println("Processing line: " + characters.map(_.toChar).mkString)
          val bits = characters map (_.toChar) selectSplit (_ != ' ')
          bits match {
            case List(method, uri, version) => {
              val m = "method: " + method.mkString
              val u = "uri: " + uri.mkString
              val v = "version: " + version.mkString
              //              some(ByteBuffer.wrap(characters.toArray))
              some(encoder.encode(CharBuffer.wrap(List(m, u, v).mkString("\n") + "\n")))
            }
            case _ => none
          }
        })) | encoder.encode(CharBuffer.wrap("Bad request.\n"))
        println("Produced: " + r)
        r
    }

    val server = Server(9000)(thing)
    for (i <- 0 to 10) {
      println("tick.")
      server.step_!
    }

    server.close_!
    println("The end.")
  }
}
