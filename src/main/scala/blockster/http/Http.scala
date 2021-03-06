package blockster
package http

import scalaz._
import Scalaz._
import IterV._

import scalaz.http.{Versions, Version, GeneralHeaders, EntityHeaders}
import scalaz.http.request.{Methods, Method, Line, Uri, RequestHeader, RequestHeaders, Request}

import Iteratees._

object Http {
  val helpers = new Versions with Methods with GeneralHeaders with RequestHeaders with EntityHeaders
  import helpers._

  def parseMethod(bytes: List[Char]): Option[Method] = ListMethod(bytes)

  def parseVersion(bytes: List[Char]): Option[Version] = ListVersion(bytes)

  def parseUri(bytes: List[Char]): Option[Uri] = bytes.toNel >>= { (chars: NonEmptyList[Char]) =>
      val (a, b) = chars.list.span(_ != '?')
      lazy val c = b.drop(1)
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

  def parseRequest: IterV[Byte, Option[Request[Stream]]] = for (requestLine <- uptoCRLF; headers <- headerLines) yield {
    import scalaz.http.request.RequestHeader.requestHeaderValue
    // TODO pass a charset through as the headers can modify the charset used for latter headers (and the body)
    lazy val hs = (headers | Nil) ∗ { bs => requestHeaderValue(bs ∘ (_.toChar)).toList }
    (requestLine >>= Http.parseLine _) ∘ { l => Request.request(l, hs, Stream.empty) }
  }
}
