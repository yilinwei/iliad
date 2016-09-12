package iliad
package algebra

import iliad.algebra.syntax.vector._

import org.scalatest._
import org.scalatest.prop._

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

import org.typelevel.discipline.scalatest._
import cats.laws.discipline.FunctorTests

import arbitrary._

class Line3Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks 
    with Inside with Discipline {

  implicit val floatArbitrary: Arbitrary[Float] = Arbitrary { Gen.choose(-100f, 100f).filter(_ != 0f) }

  checkAll("Line3[Float]", FunctorTests[Line3].functor[Float, Float, Float])

  //TODO: check if contains method is in use
  ignore("should contain points on the line") {
    val l = Line3(v"0.0 0.0 0.0", v"1.0 0.0 0.0")

    forAll(Gen.choose(-10.0, 10.0)) { d => 
      val p = v"$d 0.0 0.0"
      l.contains(p) should be(true)
    }
  }

  test("toString is expected") {
    Line3(v"1 2 3", v"0 1 0").toString should ===("Line3(Vector(1, 2, 3), Vector(0, 1, 0))")
  }
}
