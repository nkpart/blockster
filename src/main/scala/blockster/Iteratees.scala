package blockster

import scalaz.http.Version
import scalaz.http.request.{Uri, Method, Line, Request}
import scalaz._
import Scalaz._
import IterV._
import java.nio.{CharBuffer, ByteBuffer}

object Iteratees {
  def upto[E](p: E => Boolean) = {
    def step(es: List[E])(s: Input[E]): IterV[E, List[E]] = {
      s(el = e => {
        if (p(e)) {Done(es.reverse, IterV.El[E](e))} else {Cont(step(e :: es))}
      },
        empty = Cont(step(es)),
        eof = Done(es.reverse, EOF[E]))
    }
    Cont(step(Nil))
  }
}