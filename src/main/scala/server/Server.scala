package server

import java.net.InetSocketAddress
import scalaz._
import scalaz.Iteratee._
import Scalaz._
import java.nio.channels.{SocketChannel, SelectionKey, ServerSocketChannel, Selector}
import java.nio.{CharBuffer, ByteBuffer}

object RichByteBuffer {
  case class RichByteBuffer(b: ByteBuffer) {
    def getInput: Input[Byte] = {
      if (b.remaining > 0) {
        // Forcing evaluation. Do not inline.
        val byte = b.get
        El(byte)
      } else {
        EOF[Byte]
      }
    }
  }
  implicit def to(b: ByteBuffer): RichByteBuffer = RichByteBuffer(b)
}

import RichByteBuffer._

class Server private(val server: ServerSocketChannel, val selector: Selector, val iter: Iteratee[Byte, ByteBuffer]) {
  val serverKey = server.register(selector, SelectionKey.OP_ACCEPT)

  def accept {
    var client: SocketChannel = server.accept()
    client.configureBlocking(false)
    val clientKey: SelectionKey = client.register(selector, SelectionKey.OP_READ)
    clientKey.attach(iter)
  }


  def feedBuffer[C](buffer: ByteBuffer, iter: Iteratee[Byte, C]): Iteratee[Byte, C] = {
    var it = iter
    while (buffer.remaining > 0 && !it.fold(done = (_, _) => true, cont = _ => false)) {
      it = it.fold(done = (a, i) => {
        val a_ = a
        val i_ = i
        Done(a, i)
      }, cont = k => k(buffer.getInput))
    }
    it
  }

  def handle(key: SelectionKey) {
    println(key)
    val channel: SocketChannel = key.channel.asInstanceOf[SocketChannel]
    val iter = key.attachment.asInstanceOf[Iteratee[Byte, ByteBuffer]]

    def writeBuffer(b: => ByteBuffer, i: => Input[Byte]) {
      val ii = i
      val bb = b
      println("Writing: " + bb)
      channel.write(bb)
      channel.close
      key.cancel
      println("Done write.")
    }

    iter.fold(done = writeBuffer, cont = k => {
      val kk = k
      val b = ByteBuffer.allocate(512)
      val readBytes = channel.read(b)
      println(readBytes)
      b.rewind
      val newIter = (readBytes == 0) ? kk(EOF[Byte]) | feedBuffer(b, Cont(kk))
      newIter.fold(done = writeBuffer, cont => {key.attach(newIter); ()})
      ()
    })
    println("Done handle.")
  }

  def step_! {
    selector.select()
    val keys = selector.selectedKeys
    val (acceptable, rest) = keys.partition(k => k == serverKey && k.isAcceptable)
    val readable = rest.filter(k => k.isReadable)
    keys.clear

    acceptable.foreach {key => accept}
    readable.foreach { handle }
  }

  def close_! {
    selector.select()
    selector.selectedKeys.foreach { key =>
        key.channel.close
        key.cancel
    }
    server.close
  }
}

object Server {
  def apply(port: Int)(iter: Iteratee[Byte, ByteBuffer]) = {
    val server = ServerSocketChannel.open()

    server.socket.bind(new InetSocketAddress(9000))
    server.configureBlocking(false)

    val selector = Selector.open()
    new Server(server, selector, iter)
  }
}
