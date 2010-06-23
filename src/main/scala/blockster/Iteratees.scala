package blockster

import scalaz.http.Version
import scalaz.http.request.{Uri, Method, Line, Request}
import scalaz._
import Scalaz._
import IterV._
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.channels.SocketChannel

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


  val CR: Byte = 13
  val LF: Byte = 10

  val uptoCRLF: IterV[Byte, Option[List[Byte]]] = for (chars <- upto[Byte](_ == CR); _ <- head[Byte]; next <- head[Byte]) yield {
    next match {
      case Some(LF) => some(chars)
      case _ => none
    }
  }

  val headerLines = manyUntil(uptoCRLF)(_.isEmpty)

  // Repeat the iteratee until it meets the condition. Discards the result that met it
  def manyUntil[E, A](startIter: IterV[E, Option[A]])(p: A => Boolean): IterV[E, Option[List[A]]] = {
    def step(xs: List[A])(inner: IterV[E, Option[A]])(input: Input[E]): IterV[E, Option[List[A]]] = {
      input(empty = Cont(step(xs)(inner)),
        // TODO maybe should check done-ness of the inner first, could fail here if it isn't finished
        eof = Done(some(xs), EOF[E]),
        el = e => {
          inner.fold(done = (c, i) => {
            // Done and got a line, empty line means terminate.
            c match {
              case Some(x) if p(x) => Done(some(xs), input)
              case Some(x) => step(x :: xs)(startIter)(input)
              case None => Done(none, input)
            }
          }, cont = k => {
            Cont(step(xs)(k(El(e))))
          })
        }
        )
    }

    Cont(step(Nil)(startIter))
  }

  def bufferWriter(bytes: ByteBuffer) = {
    def step(remainingBytes: ByteBuffer)(input: Input[SocketChannel]): IterV[SocketChannel, Either[ByteBuffer, Unit]] = {
      if (remainingBytes.hasRemaining) {
        input(
          empty = Cont(step(remainingBytes)),
          eof = Done(Left(remainingBytes), EOF[SocketChannel]),
          el = channel => {
            val written = channel.write(remainingBytes)
            val unwrittenBytes = remainingBytes.slice
            if (unwrittenBytes.hasRemaining) {
              Cont(step(unwrittenBytes))
            } else {
              Done(Right(()), EOF[SocketChannel])
            }
          }
        )
      } else {
        Done(Right(()), input)
      }
    }
    Cont(step(bytes))
  }

}