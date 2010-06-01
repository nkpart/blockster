import java.net.InetSocketAddress
import java.nio.channels._
import java.nio.charset.{CharsetEncoder, Charset}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Set => JSet}
import java.lang.{Iterable => JIterable}
import scalaz._
import Scalaz._
import Iteratee._

class Server(val server: ServerSocketChannel, val selector: Selector, val iteratee: Iteratee[Byte, ByteBuffer]) {
  val serverKey = server.register(selector, SelectionKey.OP_ACCEPT)

  def step_! = {
    selector.select()
    val keys = selector.selectedKeys
    val (acceptable, rest) = keys.partition(k => k == serverKey && k.isAcceptable)
    val readable = rest.filter(k => k.isReadable)
    keys.clear
    State(this, iteratee, acceptable, readable)
  }
}

object Server {
  def apply(port: Int)(iteratee: Iteratee[Byte, ByteBuffer]) = {
    val server = ServerSocketChannel.open()

    server.socket.bind(new InetSocketAddress(9000))
    server.configureBlocking(false)

    val selector = Selector.open()
    new Server(server, selector, iteratee)
  }
}

case class State(s: Server, iteratee: Iteratee[Byte, ByteBuffer], acceptable: JIterable[SelectionKey], readable: JIterable[SelectionKey]) {
  def accept_! {
    acceptable.foreach {
      key =>
        var client: SocketChannel = s.server.accept()
        client.configureBlocking(false)
        val clientKey: SelectionKey = client.register(s.selector, SelectionKey.OP_READ)
        clientKey.attach(iteratee)
    }
  }

  def read_! {
    def r(ch: SocketChannel): Input[Byte] = {
      val b = ByteBuffer.allocate(1)
      if (ch.read(b) > 0) {
        b.rewind
        El(b.get(0))
      } else {
        EOF[Byte]
      }
    }

    readable.foreach {
      (key: SelectionKey) =>
        val channel: SocketChannel = key.channel.asInstanceOf[SocketChannel]
        val iteratee = key.attachment.asInstanceOf[Iteratee[Byte, ByteBuffer]]
        iteratee.fold(done = (i, _) => {
          channel.write(i)
          key.cancel
          channel.close
          ()
        }, cont = k => {
          val newIter = k(r(channel))
          key.attach(newIter)
          ()
        })
    }
  }
}


object Main {
  def main(args: Array[String]) {
    println("Starting")

    var charset: Charset = Charset.forName("UTF-8")

    def upto[E](p: E => Boolean) = {
      def step(es: List[E])(s: Input[E]): Iteratee[E, List[E]] = {
        s(el = e => {
            val next = es ++ List(e)
            if (p(e)) { Done(next, s) } else { Cont(step(next)) }
          },
          empty = Cont(step(es)),
          eof = Done(es, EOF[E]))
      }
      Cont(step(Nil))
    }

//    def accept[E](p: E) = {
//      def step(s: Input[E]): Iteratee[E, Boolean] = {
//        s(el = e => Done(e == p, Iteratee.Empty[E]),
//          empty = Cont(step),
//          eof = Done(false, EOF[E]))
//      }
//      Cont(step)
//    }

    val CR: Byte = 13
    val LF: Byte = 10

    val line = upto(LF)

    val thing = line ∘ { xs =>
      val decoder = charset.newDecoder
      val str: CharBuffer = decoder.decode(ByteBuffer.wrap(xs.toArray))
      val result: String = str.array.reverse ++ List('\n')
      val encoder: CharsetEncoder = charset.newEncoder
      encoder.encode(CharBuffer.wrap(result))
    }

    val server = Server(9000) {
      thing
    }

    while (true) {
      println("tick.")
      val state = server.step_!
      state.accept_!
      state.read_!
    }

    println("The end.")
  }
}
