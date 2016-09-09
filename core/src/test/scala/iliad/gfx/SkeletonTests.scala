package iliad
package gfx

import scala.{Vector => SVector}

import iliad.algebra._
import iliad.algebra.syntax.vector._
import iliad.algebra.syntax.axisAngle._
import spire.implicits._

import org.scalatest._

import Skeleton._

class SkeletonTests extends FunSuite with Matchers {

  val x = Vector.basis[X, _3D, Float]
  val y = Vector.basis[Y, _3D, Float]
  
  val dummy = Bone(10f, x, y)
//  val z = Vector.basis[Z, _3D, Float]

  test("skeleton should have expected size") {
    Joint(
      dummy,
      Children(
        Joint(
          dummy,
          Children(
            dummy,
            dummy,
            dummy
          )
        ),
        dummy
      )
    ).size shouldBe 6
  }

  val ordered = Joint(
    Bone(1f, x, y),
    Children(
      Joint(
        Bone(2f, x, y),
        SVector(
          Bone(3f, x, y),
          Bone(4f, x, y)
        )
      ),
      Joint(
        Bone(5f, x, y),
        Children.empty
      ),
      Bone(6f, x, y)
    )
  )


  test("skeleton should traverse in left to right order") {
    ordered.foldLeft(List.empty[Int])((b, a) => a.length.toInt :: b).reverse should contain theSameElementsAs (1 to 6)
  }

  test("skeleton should get expected paths") {
    
    ordered.foldLeftPath(Map.empty[Int, List[Int]]) { (m, idx, path) =>
      m.contains(idx) shouldBe false
      m + (idx -> path)
    } shouldBe Map(
      0 -> List.empty,
      1 -> List(0),
      2 -> List(1, 0),
      3 -> List(1, 0),
      4 -> List(0),
      5 -> List(0)
    )
  }
}
