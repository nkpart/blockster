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

  val CR: Byte = 13
  val LF: Byte = 10

  val line: Iteratee[Byte, Option[List[Byte]]] = for (chars <- upto[Byte](_ == CR); next <- head[Byte]) yield {
    next match {
      case Some(LF) => some(chars)
      case _ => none
    }
  }
}
