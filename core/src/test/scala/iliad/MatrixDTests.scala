package iliad

import iliad.syntax.matrixd._
import iliad.syntax.vectord._
import arbitrary._


import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import cats._
import cats.laws.discipline._
import cats.kernel.laws._
import cats.implicits._

import shapeless._
import shapeless.ops.nat._

import spire.implicits._

class MatrixDTests extends FunSuite with GeneratorDrivenPropertyChecks {

    implicitly[spire.algebra.MultiplicativeSemigroup[Mat4f]]
    implicitly[spire.algebra.MultiplicativeSemigroup[MatrixD[nat._4, nat._4, Double]]]
    implicitly[spire.algebra.Group[MatrixD[nat._4, nat._4, Float]]]
    implicitly[spire.algebra.Group[MatrixD[nat._4, nat._4, Double]]]
    implicitly[MatrixProduct[nat._4, nat._4, nat._1, Float]]
    implicitly[MatrixProduct[nat._4, nat._4, nat._1, Double]]
    implicitly[MatrixProduct[nat._4, nat._4, nat._4, Float]]
    implicitly[MatrixProduct[nat._4, nat._4, nat._4, Double]]

  test("matrix context syntax is equivalent to explicit creation") {
    val m0 = m"""1 2 3
                 4 5 6"""
    val m1 = MatrixD.sized(3, 2, Vector(1, 2, 3, 4, 5, 6))
    assert(m0 === m1)
  }

  test("matrix toString prints matrix in row order") {
    val nl = System.lineSeparator()
    val m = MatrixD.sized(3, 2, Vector(1, 2, 3, 4, 5, 6))
    val s = s"""MatrixD:${nl}1 | 2 | 3${nl}4 | 5 | 6"""
    assert(m.toString == s)
  }

  test("width returns width of matrix") {
    val m = MatrixD.sized(3, 2, Vector(1, 2, 3, 4, 5, 6))
    assert(m.width == 3)
  }

  test("height returns height of matrix") {
    val m = MatrixD.sized(3, 2, Vector(1, 2, 3, 4, 5, 6))
    assert(m.height == 2)
  }

  test("toArray returns contents in row order") {
    val v0: Vector[Int] = Vector(1, 2, 3, 4, 5, 6)
    val v1: Vector[Int] = MatrixD.sized(3, 2, v0).toArray.toVector
    assert(v1 == v0)
  }

  test("identity matrix is a diagonal matrix of ones") {
    val v: Vector[Int] = Vector(1, 0, 0, 0, 1, 0, 0, 0, 1)
    assert(MatrixD.identity[nat._3, Int].toArray.toVector == v)
  }

  {
    test("identity matrix multiplied by a vector returns the vector") {
      val id = MatrixD.identity[nat._4, Int]
      val vectors = VectorDGen[nat._4, Int](Gen.choose(-1000, 1000))
      forAll(vectors) { v =>
        assert(id.times(v) === v)
      }
    }

    test("identity matrix multiplied by a matrix returns the matrix") {
      val id = MatrixD.identity[nat._4, Int]
      val matrices = MatrixDGen[nat._4, nat._4, Int](Gen.choose(-1000, 1000))
      forAll(matrices) { m =>
        assert(id.times(m) === m)
      }
    }

    test("inverse of the identity matrix is the identity matrix") {
      val id = MatrixD.identity[nat._4, Int]
      assert(id.inverse() === id)
    }
  }
}
