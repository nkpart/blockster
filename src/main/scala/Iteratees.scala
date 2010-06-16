package blockster

import scalaz.http.Version
import scalaz.http.request.{Uri, Method, Line, Request}
import scalaz._
import Scalaz._
import Iteratee._
import java.nio.{CharBuffer, ByteBuffer}

object Iteratees {
  def f[E, A, B](ma: Iteratee[E, A], mb: Iteratee[E, B]): Iteratee[E, A] = for (a <- ma; _ <- mb) yield (a)

  def accumulate[E, S](s: S)(f: E => S => Iteratee[E, S])(i: Input[E]): Iteratee[E, S] = {
    i(
      el = e => f(e)(s),
      empty = Cont(accumulate(s)(f) _),
      eof = Done(s, EOF[E])
      )
  }
}