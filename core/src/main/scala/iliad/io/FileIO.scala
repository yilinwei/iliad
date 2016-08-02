package iliad
package io

import java.io.{InputStream, IOException}

import cats._
import cats.implicits._
import cats.data._

import File._

final class FileIO[A] private[io] (file: Option[File.Descriptor], private[io] val eval: FileIO.Evaluate[A]) {

  import FileIO.MEval._

  import ops.file._

  private def modify[B](f: FileIO.Evaluate[A] => FileIO.Evaluate[B]): FileIO[B] =
    new FileIO(file, f(eval))

  private def cached: FileIO.Evaluate[Unit] = file.map { f =>
    for {
      dir <- ask
      cache <- get
      f = file.get
      _  <- if(cache.contains(f)) pure(()) else set(cache + (f -> Eval.later(dir.open(f))))
    } yield ()
  }.getOrElse(pure(()))
  
  private def streaming[A](f: InputStream => FileIO.Evaluate[A]): FileIO[A] = modify {
    _.flatMap { _ =>
      get.flatMap { cache =>
        val r = for {
          ff <- file
          att <- cache.get(ff)
        } yield att.value match {
          case Xor.Left(err) => raiseError[A](err)
          case Xor.Right(is) => f(is)
        }
        //open stream if needed
        r.getOrElse(cached.flatMap(_ => streaming(f).eval))
      }
    }
  }

  def map[B](f: A => B): FileIO[B] = modify(_.map(f))

  def flatMap[B](f: A => FileIO[B]): FileIO[B] = modify {
    _.flatMap(a => f(a).eval)
  }

  def read[AA](n: Int)(implicit fileRead: FileRead[AA]): FileIO[List[AA]] = streaming { is =>
    val len = n * fileRead.byteSize
    val bytes = new Array[Byte](len)
    val obtained = is.read(bytes, 0, len)
    if(obtained == len) {
      pure((0 until n).map { i =>
        val arr = new Array[Byte](fileRead.byteSize)
        Array.copy(bytes, i * fileRead.byteSize, arr, 0, fileRead.byteSize)
        fileRead.decode(arr)
      }.toList)
    } else raiseError(new IllegalStateException(s"stream for ${file} does not have $n elements to read"))
  }

  def read1[AA](implicit fileRead: FileRead[AA]): FileIO[AA] =  read[AA](1).map(_.head)

  def skip[AA](n: Int): FileIO[Unit] = streaming { is =>
    val skipped = is.skip(n)
    if(skipped.toInt == n) pure(()) else raiseError(new IllegalStateException(s"stream for ${file} does not have $n elements to skip"))
  }

  def skip1[AA]: FileIO[Unit] = skip(1)

  def chunk[AA](n: Int): FileIO[Chunk[AA]] = streaming { is =>
    val bytes = new Array[Byte](n)
    val chunked = is.read(bytes, 0, n)
    if(chunked == n) pure(new Chunk[AA](bytes)) else raiseError(new IllegalStateException(s"stream for ${file} does not have expected $n elements to chunk"))
  }

  def chunk1[AA]: FileIO[Chunk[AA]] = chunk(1)

  def recoverWith(f: Throwable => FileIO[A]): FileIO[A] = modify(fa => handleErrorWith(fa)(f andThen (_.eval)))

  def unsafePerform(implicit dir: File.Dir): Eval[Throwable Xor A] =
    eval.run(dir).value.run(Map.empty).map {
      case (open, a) =>
        open.mapValues(_.value.map(_.close))
        a
    }   
}    

private[io] sealed trait FileIOIsMonad extends Monad[FileIO] {
  def flatMap[A, B](fa: FileIO[A])(f: A => FileIO[B]): FileIO[B] = fa.flatMap(f)
  def pure[A](a: A): FileIO[A] = FileIO.pure(a)
  override def map[A, B](fa: FileIO[A])(f: A => B): FileIO[B] = fa.map(f)
}

private[io] final class EvaluateMonadErrorStateReader
    extends MonadState[FileIO.Evaluate, Map[File.Descriptor, Eval[IOException Xor InputStream]]]
    with MonadReader[FileIO.Evaluate, File.Dir]
    with MonadError[FileIO.Evaluate, Throwable] {

  def pure[A](x: A): FileIO.Evaluate[A] = ReaderT.pure(x)
  def flatMap[A, B](fa: FileIO.Evaluate[A])(f: A => FileIO.Evaluate[B]): FileIO.Evaluate[B] = fa.flatMap(f)

  def handleErrorWith[A](fa: FileIO.Evaluate[A])(f: Throwable => FileIO.Evaluate[A]): FileIO.Evaluate[A] = ReaderT { dir =>
    (fa.run andThen { xor =>
      xor.recoverWith { case t => f(t).run(dir) }
    })(dir)
  }

  def raiseError[A](e: Throwable): FileIO.Evaluate[A] =
    XorT[State[Map[File.Descriptor, Eval[IOException Xor InputStream]], ?], Throwable, A](State.pure(e.left)).liftT[ReaderT[?[_], File.Dir, ?]]

  def ask: FileIO.Evaluate[File.Dir] = ReaderT.ask
  def local[A](f: File.Dir => File.Dir)(fa: FileIO.Evaluate[A]): FileIO.Evaluate[A] = ReaderT.local(f)(fa)


  def get: FileIO.Evaluate[Map[File.Descriptor, Eval[IOException Xor InputStream]]] =
    State.get[Map[File.Descriptor, Eval[IOException Xor InputStream]]].liftT[XorT[?[_], Throwable, ?]].liftT[ReaderT[?[_], File.Dir, ?]]
      
  def set(s: Map[File.Descriptor, Eval[IOException Xor InputStream]]): FileIO.Evaluate[Unit] =
    State.set(s).liftT[XorT[?[_], Throwable, ?]].liftT[ReaderT[?[_], File.Dir, ?]]
}

object FileIO {

  import ops.file._


  type Evaluate[A] = ReaderT[XorT[State[Map[File.Descriptor, Eval[IOException Xor InputStream]], ?], Throwable, ?], File.Dir, A]

  private[io] val MEval = new EvaluateMonadErrorStateReader

  def pure[A](a: A): FileIO[A] = new FileIO(None, MEval.pure(a))

  def file(file: File.Descriptor): FileIO[Unit] =
    new FileIO(Some(file), MEval.pure(()))

  def terminate(err: Throwable): FileIO[Unit] =
    new FileIO(None, MEval.raiseError(err))



  implicit val M: Monad[FileIO] = new FileIOIsMonad {} 

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


    val test2 = java.nio.ByteBuffer.allocate(4)
    test2.putInt(1)
  //  test2.putInt(4)
    test2.rewind()

    val d1 = new java.io.ByteArrayInputStream(test.array())
    val d2 = new java.io.ByteArrayInputStream(test2.array())

    implicit val directory = File.Dir.from(
      f1 -> d1,
      f2 -> d2  
    )

    val ff1 = FileIO.file(f1)

    val ff2 = FileIO.file(f2)

    val res = ff2.read[Int](2).recoverWith(t => ff1.read[Int](2))

/*    val res = for {
      foo <- ff1.read[Int](1)
      foo2 <- ff1.read[Int](1)
      bar <- ff2.read[Int](2)
    } yield s"$foo $foo2 $bar"   */

    println(res.unsafePerform.value)

  }
}*/
