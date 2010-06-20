package blockster
package http

import scalaz._
import Scalaz._
import Iteratee._

import scalaz.http.{Versions, Version}
import scalaz.http.request.{Methods, Method, Line, Uri}

import Iteratees._

object Http {
  val x = new Versions with Methods
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

    /**
   * Converts a list of characters of the form "abc:def" into a potential request header and non-empty value split at
   * the colon (:).
   */
  def headerValue[T](cs: List[Char])(implicit f: List[Char] => Option[T]): Option[(T, NonEmptyList[Char])] =
    cs span (_ != ':') match {
      case (n, v) => {
        f(n) ∗ (h =>
          (v.dropWhile(x => x == ':' || isWhitespace(x))).toNel map (v => (h, v)))
      }
    }

  def lines: Iteratee[Byte, Option[List[List[Byte]]]] = {
    def step(xs: List[List[Byte]])(inner: Iteratee[Byte, Option[List[Byte]]])(input: Input[Byte]): Iteratee[Byte, Option[List[List[Byte]]]] = {
      input(empty = Cont(step(xs)(inner)),
            eof = Done(some(xs), EOF[Byte]), // TODO maybe should check done-ness of the inner first, could fail here if it isn't finished
            el = byte => {
         inner.fold(done = (c, i) => {
           // Done and got a line, empty line means terminate.
            c match {
              case Some(List(CR)) => Done(some(xs), input)
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

  val line: Iteratee[Byte, Option[List[Byte]]] = for (chars <- upto[Byte](_ == CR); next <- head[Byte]) yield {
    next match {
      case Some(LF) => some(chars)
      case _ => none
    }
  }
}
