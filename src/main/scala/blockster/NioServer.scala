package blockster

import java.net.InetSocketAddress
import scalaz._
import scalaz.IterV._
import Scalaz._
import scala.collection.JavaConversions._
import java.nio.channels.{SocketChannel, SelectionKey, ServerSocketChannel, Selector}
import java.nio.{CharBuffer, ByteBuffer}

import Iteratees._

sealed trait KeyState
case class Reading(iter: IterV[Byte, ByteBuffer]) extends KeyState
case class Writing(iter: IterV[SocketChannel, Unit]) extends KeyState

class NioServer private(val server: ServerSocketChannel, val selector: Selector, val iter: IterV[Byte, ByteBuffer]) {
  val serverKey = server.register(selector, SelectionKey.OP_ACCEPT)

  def accept {
    var client: SocketChannel = server.accept()
    client.configureBlocking(false)
    val clientKey: SelectionKey = client.register(selector, SelectionKey.OP_READ)
    clientKey.attach(Reading(iter))
  }

  def handleRead(key: SelectionKey) {
    val channel: SocketChannel = key.channel.asInstanceOf[SocketChannel]
    val keyState = key.attachment.asInstanceOf[KeyState]

    def writeBuffer(b: => ByteBuffer, i: => Input[Byte]) {
      // Force evaluation.
      val ii = i // ignored

      val iter = Iteratees.bufferWriter(b)

      iter.fold(done = (c, i) => Done(c, i), cont = k => {
        k(El(channel))
      })

      channel.close
      key.cancel
    }

    keyState match {
      case Reading(iter) => {
        iter.fold(done = writeBuffer, cont = k => {
          val kk = k
          val b = ByteBuffer.allocate(512)
          // Assumed that the key is readable. If no bytes were read, the input stream is finished
          val readBytes = channel.read(b)
          b.rewind
          val newIter = (readBytes == 0) ? kk(EOF[Byte]) | feedBuffer(b, Cont(kk))
          newIter.fold(done = writeBuffer, cont => {key.attach(Reading(newIter)); ()})
          ()
        })
      }
      case Writing(iter) => {
        ()
      }
    }
  }

  def step() {
    selector.select()
    val keys = selector.selectedKeys
    val (acceptable, rest) = keys.partition(k => k == serverKey && k.isAcceptable)
    val readable = rest.filter(k => k.isReadable)
    keys.clear

    acceptable.foreach {_ => accept}
    readable.foreach(handleRead)
  }

  def close() {
    selector.select()
    selector.selectedKeys.foreach {
      key =>
        key.channel.close
        key.cancel
    }
    server.close
  }
}

object NioServer {
  def apply(port: Int)(iter: IterV[Byte, ByteBuffer]) = {
    val server = ServerSocketChannel.open()

    server.socket.bind(new InetSocketAddress(port))
    server.configureBlocking(false)

    val selector = Selector.open()
    new NioServer(server, selector, iter)
  }
}
