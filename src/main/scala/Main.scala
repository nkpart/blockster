import java.net.InetSocketAddress
import java.nio.channels._
import java.nio.charset.{CharsetEncoder, Charset}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Set => JSet}
import java.lang.{Iterable => JIterable}
import scalaz._
import Scalaz._
import Iteratee._

class Server private(val server: ServerSocketChannel, val selector: Selector, val iteratee: Iteratee[ByteBuffer, ByteBuffer]) {
  val serverKey = server.register(selector, SelectionKey.OP_ACCEPT)

  def accept {
    var client: SocketChannel = server.accept()
    client.configureBlocking(false)
    val clientKey: SelectionKey = client.register(selector, SelectionKey.OP_READ)
    clientKey.attach(iteratee)
  }

  def handle(key: SelectionKey) {
    val channel: SocketChannel = key.channel.asInstanceOf[SocketChannel]
    val iteratee = key.attachment.asInstanceOf[Iteratee[ByteBuffer, ByteBuffer]]
    iteratee.fold(done = (i, _) => {
      channel.write(i)
      key.cancel
      channel.close
      ()
    }, cont = k => {
      val b = ByteBuffer.allocate(512)
      val input: Input[ByteBuffer] = if (channel.read(b) > 0) {
        b.rewind
        El(b)
      } else {EOF[ByteBuffer]}
      val newIter = k(input)
      key.attach(newIter)
      ()
    })
  }

  def step_! {
    selector.select()
    val keys = selector.selectedKeys
    val (acceptable, rest) = keys.partition(k => k == serverKey && k.isAcceptable)
    val readable = rest.filter(k => k.isReadable)
    keys.clear

    acceptable.foreach {key => accept}
    readable.foreach {key => handle(key)}
  }
}

object Server {
  def apply(port: Int)(iteratee: Iteratee[ByteBuffer, ByteBuffer]) = {
    val server = ServerSocketChannel.open()

    server.socket.bind(new InetSocketAddress(9000))
    server.configureBlocking(false)

    val selector = Selector.open()
    new Server(server, selector, iteratee)
  }
}

object Main {
  def main(args: Array[String]) {
    println("Starting")

    var charset: Charset = Charset.forName("UTF-8")

    def readInput(b: ByteBuffer): Input[Byte] = {
      if (b.remaining > 0) {
        El(b.get)
      } else {
        EOF[Byte]
      }
    }
    def toArray(b: ByteBuffer): Array[Byte] = {
      val array = Array.ofDim[Byte](b.remaining)
      b.get(array)
      array
    }

    def blah[C](it: Iteratee[Byte, C]): Iteratee[ByteBuffer, C] = {
      it.fold(
        done = (c, rest) => {Done(c, rest.apply(empty = Iteratee.Empty[ByteBuffer], el = b => El[ByteBuffer](ByteBuffer.wrap(Array(b))), eof = EOF[ByteBuffer]))},
        cont = (f: (Input[Byte] => Iteratee[Byte, C])) => {
          Cont((input: Input[ByteBuffer]) => {
            input(
              empty = blah(f(Iteratee.Empty[Byte])),
              eof = blah(f(EOF[Byte])),
              el = buffer => {
                val bytes = toArray(buffer)
                blah(bytes.foldLeft(Cont(f))((i, byte) => {
                  i.fold(cont = f => f(El(byte)), done = (c, i) => Done(c, i))
                }))
              }
              )
          })
        }
        )
    }

    def upto[E](p: E => Boolean) = {
      def step(es: List[E])(s: Input[E]): Iteratee[E, List[E]] = {
        s(el = e => {
          val next = es ++ List(e)
          if (p(e)) {Done(next, s)} else {Cont(step(next))}
        },
          empty = Cont(step(es)),
          eof = Done(es, EOF[E]))
      }
      Cont(step(Nil))
    }

    val CR: Byte = 13
    val LF: Byte = 10

    val line = upto[Byte](_ == LF)

    val thing = line ∘ {
      xs =>
        println(xs.length)
        val decoder = charset.newDecoder
        val str: CharBuffer = decoder.decode(ByteBuffer.wrap(xs.toArray))
        val result = str.array.filter(b => b != '\r' && b != '\n').reverse ++ List('\n')
        val encoder: CharsetEncoder = charset.newEncoder
        encoder.encode(CharBuffer.wrap(result))
    }

    val server = Server(9000) {blah(thing)}
    for (i <- 0 to 10) {
      println("tick.")
      server.step_!
    }
    server.server.close
    println("The end.")
  }
}
