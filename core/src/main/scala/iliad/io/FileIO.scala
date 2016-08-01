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

//TODO: pure
final class FileIO[A] private[io] (val file: File.Descriptor, val stream: Eval[InputStream], val eval: FileIO.Evaluate[A]) {

  import ops.file._

  private def evalModify[B](f: FileIO.Evaluate[A] => FileIO.Evaluate[B]): FileIO[B] =
    new FileIO(file, stream, f(eval))

  def read[AA](n: Int)(implicit fileRead: FileRead[AA]): FileIO[List[AA]] = evalModify {
    _.get.map { cache =>
      //TODO: Need to deal with errors better here
      val is = cache(file).value
      val len = n * fileRead.byteSize
      val bytes = new Array[Byte](len)
      is.read(bytes, 0, len)
      (0 until n).map { i =>
        val arr = new Array[Byte](fileRead.byteSize)
        Array.copy(bytes, i * fileRead.byteSize, arr, 0, fileRead.byteSize)
        fileRead.decode(arr)
      }.toList
    }
  }

  def read1[AA](implicit fileRead: FileRead[AA]): FileIO[AA] =  read[AA](1).map(_.head)

  def map[B](f: A => B): FileIO[B] = evalModify(_.map(f))

  def flatMap[B](f: A => FileIO[B]): FileIO[B] = evalModify {
    _.flatMap { a =>
      val fio = f(a)
      //TODO: use modify once it's released
      StateT[Throwable Xor ?, Map[File.Descriptor, Eval[InputStream]], Unit] { cache =>
        if(cache.contains(fio.file)) (cache, ()).right else (cache + (fio.file -> fio.stream), ()).right
      }.flatMap(_ => fio.eval)
    }
  }

  def unsafePerform: Eval[Throwable Xor A] = Eval.later {
    eval.run(Map(file -> stream)).map {
      case (streams, a) =>
        //TODO: Try catch!
        streams.mapValues(_.value.close)
        a
    }
  }

}    


object FileIO {

  type Evaluate[A] = StateT[Throwable Xor ?, Map[File.Descriptor, Eval[InputStream]], A]


/*  private case class Leaf[A](file: File.Descriptor, repr: InputStream, run: ReaderT[Throwable Xor ?, InputStream, A]) extends FileIO[A]
 */
  /*

   FileIO.from("file1.stuff").read[Char](30).flatMap { char =>
      FileIO.from("file2.stuff").map(...) yada, yada
   }
   */

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
    val ff1 = new FileIO(f1, Eval.later {
      println("opened!")
      d1
    }, StateT.pure(()))

    val ff2 = new FileIO(f2, Eval.later {
      println("opened2!")
      d2
    }, StateT.pure(()))


    val res = for {
      foo <- ff1.read[Int](2)
      bar <- ff2.read[Int](2)
    } yield s"$foo $bar"   

    println(res.unsafePerform.value)

  }

}*/
