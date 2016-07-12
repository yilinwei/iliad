package iliad

import java.io.InputStream
import java.nio.ByteBuffer

import shapeless._

import cats._
import cats.free._
import cats.data._

import cats.implicits._

/**
  * Codecs must be able to compose using concat (for consecutive assets) as well as interleave?
  *  1) Must be a monad because the size of the is can affect that macro itself, much like scodec
  * 2) expect little to no processing of the elements 
  * 3) expect consecutive elements.
  * 4) also expect pure streams as well
  * 5) when using a free the F[_] used is strange?

  * Codec.Int(20) :: Codec.Float(20) should produce (Buffer[Int], Buffer[Float])
  * Codec.Vec[_3, Float](10) should produce Buffer[Float :: Float :: HNil]
  * If we want to interleave we should do something like,
  * (Codec.Int, Codec.Float)(32) (This should return a BB)
  *  Essentially our bottom type is [[Byte]]
  * We have two sections 1) is to Read, then the next is to put 
  * Our codec essentially must Pull from a source, 
  */ 



/**
  * the source of [[Bytes]] as well as the sync of bytes are different
  * we need to do very little processing for each item
  * We need to have the idea of n outputs... or rather
  *  A1 :: A2 should require a compiler for both which curry the beginning of
  *  the output
  */
sealed trait ChannelDSL[+A]

import CatsExtra._

object ChannelDSL {

  case class Open[A](id: Int)

  case object Read extends ChannelDSL[Byte]
  case class ReadN(n: Int) extends ChannelDSL[ByteBuffer]
  case class Allocate[A](n: Int) extends ChannelDSL[Open[A]]
  case class Stream(o: Open[_], n: Option[Int]) extends ChannelDSL[Unit]
  case class WriteN(o: Open[_], bb: ByteBuffer) extends ChannelDSL[Unit]
  case class Write(b: Byte) extends ChannelDSL[Unit]
  case object Remaining extends ChannelDSL[Int]
  case class Terminate(t: Throwable) extends ChannelDSL[Nothing]
}

import ChannelDSL._

object Channel {

  type Channel[A] = Free[ChannelDSL, A]


  def rest[A](implicit BO: ByteOps[A]): Channel[Open[A]] = for {
    n <- Remaining.free
    o <- if(n % BO.byteSize == 0) allocate(n) else Terminate(new IllegalStateException("required x bytes, found y")).free
    _ <- Stream(o, None).free
  } yield o
  
  /** read n values of type A
    */
  def reads[A](n: Int)(implicit BO: ByteOps[A]): Channel[List[A]] = Remaining.free.flatMap { n =>
    val s = n * BO.byteSize
    if(n < s) 
      Terminate(new IllegalStateException("")).free 
    else ReadN(s).free.map { bb =>
      (1 to n).map(_ => BO.get(bb)).toList
    }
  }
  /** read value of type A
    */
  def read[A](implicit BO: ByteOps[A]): Channel[A] = reads[A](1).map(_.head)
  def allocate[A](n: Int)(implicit BO: ByteOps[A]): Channel[Open[A]] =
    Free.liftF[ChannelDSL, Open[A]](Allocate[A](BO.byteSize * n))

  trait Source[A] {
    def read(a: A): Byte
    def readN(n: Int): ByteBuffer
  }
  //TODO: Should be a HList if possible
  trait Sink[A] {
    def allocate(n: Int): A
    def writeN(bb: ByteBuffer, a: A): Unit
    def write(b: Byte, a: A): Unit
  }

  def compile[A, B](source: Source[A], sink: Sink[B]): ReaderT[Throwable Xor ?, List[Buffer[Int]]] = ???
}




/*trait ChannelSource[A] extends PartialFunctionK[ChannelDSL, F] {
  def pipe[B](sink: ChannelSink[B])(implicit P: ChannelPipe[A, B]): FunctionK[ChannelDSL, ]
}

trait ChannelPipe[A, B] extends PartialFunctionK[ChannelDSL, F] {
}*/




/*object Codec {


  
  //TODO: Investigate
  type Codec[A] = Reader[InputStream, A]
/*
  def Int(n: Int): Codec[]*/

  object Int extends CodecBuilder[Int]
  object Int16 extends CodecBuilder[Short]
  object Float extends CodecBuilder[Float]
  
  def Vec[N <: Nat, A]: CodecBuilder[Float] = ???
}*/

trait CloseableK[F[_]] {
  def close: F[Unit]
}

private[iliad] final class CodecCloseableK extends CloseableK[Reader[InputStream, ?]] {
  def close: Reader[InputStream, Unit] = Reader(is => is.close())
}

object CloseableK {
  implicit val closeableKCodec: CloseableK[Reader[InputStream, ?]] = new CodecCloseableK
}

import MonadExtra._

