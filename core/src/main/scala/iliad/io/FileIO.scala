package iliad
package io

import java.io.InputStream

import cats._
import cats.implicits._
import cats.data._

object File {
  //TODO: Check consistency across platforms!
  case class Descriptor(path: String)
}

import File._

//TODO: IS to channel?
final class FileIO[A] private[io] (val file: Option[File.Descriptor], val stream: Option[Eval[InputStream]], val eval: FileIO.Evaluate[A]) {

  import ops.file._

  private def modify[B](f: FileIO.Evaluate[A] => FileIO.Evaluate[B]): FileIO[B] =
    new FileIO(file, stream, f(eval))

  private def cacheAdd: FileIO.Evaluate[Unit] =
    (for {
      f <- file
      s <- stream
    } yield State.modify[Map[File.Descriptor, Eval[InputStream]]] { cache =>
        if(cache.contains(f)) cache else (cache + (f -> s))
    }.liftT[XorT[?[_], Throwable, ?]]).getOrElse(XorT.pure(()))

  private def terminate[A](t: Throwable): FileIO.Evaluate[A] = XorT(State(_ -> t.left[A]))
  private def pureEval[A](a: A): FileIO.Evaluate[A] = XorT.pure(a)

  def read[AA](n: Int)(implicit fileRead: FileRead[AA]): FileIO[List[AA]] = modify { e =>
    e.value.get.liftT[XorT[?[_], Throwable, ?]].flatMap { cache =>
      val r = for {
        f <- file
        iss <- cache.get(f)
      } yield {
        val is = iss.value
        val len = n * fileRead.byteSize
        val bytes = new Array[Byte](len)
        is.read(bytes, 0, len)
       (0 until n).map { i =>
         val arr = new Array[Byte](fileRead.byteSize)
         Array.copy(bytes, i * fileRead.byteSize, arr, 0, fileRead.byteSize)
         fileRead.decode(arr)
       }.toList
      }
      r.map(pureEval).getOrElse(terminate(new IllegalStateException(s"unable to find $file in cache")))
    }
  }

  def read1[AA](implicit fileRead: FileRead[AA]): FileIO[AA] =  read[AA](1).map(_.head)

  def map[B](f: A => B): FileIO[B] = modify(_.map(f))

  def flatMap[B](f: A => FileIO[B]): FileIO[B] = modify {
    _.flatMap { a =>
      val fio = f(a)
      fio.cacheAdd.flatMap(_ => fio.eval)
    }
  }

  private def initialState: Map[File.Descriptor, Eval[InputStream]] = (for {
    f <- file
    s <- stream
  } yield Map(f -> s)).getOrElse(Map.empty)

  def unsafePerform: Eval[Throwable Xor A] =
    eval.value.run(initialState).map {
      case (open, a) =>
        open.mapValues(_.value.close)
        a
    }

}    


object FileIO {


  type Evaluate[A] = XorT[State[Map[File.Descriptor, Eval[InputStream]], ?], Throwable, A]

  def pure[A](a: A): FileIO[A] = new FileIO(None, None, XorT.pure(a))
  def from(file: File.Descriptor): FileIO[Unit] = ???
}

//Files should never stay open since the IO threads are expensive and have to be used

/*object TestFile {
  def main(args: Array[String]): Unit = {
    val f1 = File.Descriptor("foo")
    val f2 = File.Descriptor("bar")
    val test = java.nio.ByteBuffer.allocate(8)
    test.putInt(2)
    test.putInt(32)
    test.rewind()


    val test2 = java.nio.ByteBuffer.allocate(8)
    test2.putInt(1)
    test2.putInt(4)
    test2.rewind()

    val d1 = new java.io.ByteArrayInputStream(test.array())
    val d2 = new java.io.ByteArrayInputStream(test2.array())
    val ff1 = new FileIO(Some(f1), Some(Eval.later {
      println("opened!")
      d1
    }), XorT.pure(()))

    val ff2 = new FileIO(Some(f2), Some(Eval.later {
      println("opened23")
      d2
    }), XorT.pure(()))


    val res = for {
      foo <- ff1.read[Int](2)
      bar <- ff2.read[Int](2)
    } yield s"$foo $bar"   

    println(res.unsafePerform.value)

  }
}*/
