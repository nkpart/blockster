import scalaz._
import http.request.Line
import Scalaz._
import Iteratee._

package object blockster {
  implicit def InputFunctor: Functor[Input] = new Functor[Input] {
    def fmap[A, B](r: Input[A], f: A => B): Input[B] = r(empty = Iteratee.Empty[B], eof = EOF[B], el = a => El(f(a)))
  }

  implicit val LineShow: Show[Line] = new Show[Line] {
    def show(l: Line): List[Char] = {
      List(l.method.asString.toList, l.uri.path.list ++ (l.uri.queryString.map('?' :: _) | Nil), l.version.asString.toList).intersperse(List(' ')).∑
    }
  }
} 