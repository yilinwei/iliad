package iliad
package io

import org.typelevel.discipline.scalatest._
import org.scalatest._

import cats._
import cats.implicits._
import cats.data._
import cats.laws.discipline._

import org.scalacheck._
import org.scalatest.prop._

import arbitrary._

class FileIOTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks {
  implicit def fileIOEq[A : Arbitrary : Eq](implicit dir: File.Dir): Eq[FileIO[A]] =
    //TODO: Other stuff
    Eq.by[FileIO[A], A](_.unsafePerform.value.toOption.get)

  implicit def arbitraryFileIO[A](implicit arbA: Arbitrary[A]): Arbitrary[FileIO[A]] =
    Arbitrary(arbA.arbitrary.map(FileIO.pure))

  {
    implicit val dir = File.Dir.from()

    Monad[FileIO]
    checkAll("Monad[FileIO]", MonadTests[FileIO].monad[Int, Int, Int])
  }
}
