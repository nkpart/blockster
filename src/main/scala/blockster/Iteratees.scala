package blockster

import scalaz.http.Version
import scalaz.http.request.{Uri, Method, Line, Request}
import scalaz._
import Scalaz._
import Iteratee._
import java.nio.{CharBuffer, ByteBuffer}

object Iteratees {
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
}