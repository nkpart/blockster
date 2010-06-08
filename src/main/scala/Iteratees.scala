package blockster

import scalaz.http.Version
import scalaz.http.request.{Uri, Method, Line, Request}
import scalaz._
import Scalaz._
import Iteratee._
import java.nio.{CharBuffer, ByteBuffer}

object Iteratees {
  def f[E, A, B](ma: Iteratee[E, A], mb: Iteratee[E, B]): Iteratee[E, A] = for (a <- ma; _ <- mb) yield (a)

  def toCharArray(b: ByteBuffer): Array[Char] = {
    val array = Array.ofDim[Char](b.remaining)
    b.asCharBuffer.get(array)
    array
  }


  def accumulate[E, S](s: S)(f: E => S => Iteratee[E, S])(i: Input[E]): Iteratee[E, S] = {
    i(
      el = e => f(e)(s),
      empty = Cont(accumulate(s)(f) _),
      eof = Done(s, EOF[E])
      )
  }

  val ItRequestLine: Iteratee[CharBuffer, List[Char]] = {
    def next(cb: CharBuffer): Option[Char] = (cb.remaining > 0).option(cb.get)

    def step(xs: List[Char])(in: Input[CharBuffer]): Iteratee[CharBuffer, List[Char]] = {
      def el(cb: => CharBuffer): Iteratee[CharBuffer, List[Char]] = {

        val buffer = scala.collection.mutable.ArrayBuffer[Char]()
        
        var ch: Option[Char] = next(cb)
        while (ch.isDefined) {
          val char = ch.get
          if (char == '\r') {
            return Done(xs, El(cb.slice)) // TODO check for more and return EOF
          } else {
            buffer += char
          }
          ch = next(cb)
        }
        Cont(step(xs ++ buffer.toList))
      }
      in(el = el, empty = Cont(step(xs)), eof = Done(xs, EOF[CharBuffer]))
    }
    Cont(step(Nil))
  }

  //  def ItLine: Iteratee[Byte, Option[Line]] =
  //    for (method <- ItMethod) yield { Method.ListMethod(method) }

  //val UntilSpace = f(ItUntil[Char](' '), head[Char])

  //def ItMethod: Iteratee[Char, Option[Method]] = UntilSpace ∘ (Method.ListMethod)

  //def ItUri: Iteratee[Char, Option[Uri]] = UntilSpace ∘ (Uri.)

  //def ItVersion: Iteratee[Char, Option[Version]] = error("todo")
}