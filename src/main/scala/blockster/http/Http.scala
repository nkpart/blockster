package blockster
package http

import scalaz._
import Scalaz._
import IterV._

import scalaz.http.{Versions, Version, GeneralHeaders, EntityHeaders}
import scalaz.http.request.{Methods, Method, Line, Uri, RequestHeader, RequestHeaders, Request}

import Iteratees._

object Http {
  val x = new Versions with Methods with GeneralHeaders with RequestHeaders with EntityHeaders
  import x._

  def parseMethod(bytes: List[Char]): Option[Method] = ListMethod(bytes)

  def parseVersion(bytes: List[Char]): Option[Version] = ListVersion(bytes)

  def parseUri(bytes: List[Char]): Option[Uri] = bytes.toNel >>= {
    (chars: NonEmptyList[Char]) =>
      val (a, b) = chars.list.span(_ != '?')
      val c = b.drop(1)
      a.toNel ∘ {v => Uri.uri(v, c.toNel ∘ (_.list))}
  }

  def parseLine(bytes: List[Byte]): Option[scalaz.http.request.Line] = {
    val parts = bytes map (_.toChar) selectSplit (_ != ' ')
    parts match {
      case List(method, uri, version) => {
        for (p <- parseMethod(method); q <- parseVersion(version); r <- parseUri(uri)) yield {Line.line(p, r, q)}
      }
      case _ => none
    }
  }

  def parseRequest: IterV[Byte, Option[Request[Stream]]] = for (requestLine <- line; headers <- lines) yield {
    import scalaz.http.request.RequestHeader.requestHeaderValue
    // TODO pass a charset through as the headers can modify the charset used for latter headers
    lazy val hs = (headers | Nil) ∗ { bs => requestHeaderValue(bs ∘ (_.toChar)).toList }
    (requestLine >>= Http.parseLine _) ∘ { l => Request.request(l, hs, Stream.empty) }
  }


  def lines: IterV[Byte, Option[List[List[Byte]]]] = {
    def step(xs: List[List[Byte]])(inner: IterV[Byte, Option[List[Byte]]])(input: Input[Byte]): IterV[Byte, Option[List[List[Byte]]]] = {
      input(empty = Cont(step(xs)(inner)),
        eof = Done(some(xs), EOF[Byte]), // TODO maybe should check done-ness of the inner first, could fail here if it isn't finished
        el = byte => {
          inner.fold(done = (c, i) => {
            // Done and got a line, empty line means terminate.
            c match {
              case Some(Nil) => Done(some(xs), input)
              case Some(x) => step(x :: xs)(line)(input)
              case None => Done(none, input)
            }
          }, cont = k => {
            Cont(step(xs)(k(El(byte))))
          })
        }
        )
    }

    Cont(step(Nil)(line))
  }

  val CR: Byte = 13
  val LF: Byte = 10

  val line: IterV[Byte, Option[List[Byte]]] = for (chars <- upto[Byte](_ == CR); _ <- head[Byte]; next <- head[Byte]) yield {
    next match {
      case Some(LF) => some(chars)
      case _ => none
    }
  }
}
