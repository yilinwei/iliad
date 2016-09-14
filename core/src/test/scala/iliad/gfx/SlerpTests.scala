package iliad
package gfx

import iliad.algebra._
import iliad.algebra.syntax.vector._
import iliad.algebra.syntax.axisAngle._
import spire.implicits._

import org.scalatest._

class SlerpTests extends FunSuite {

  val x = Vector.basis[X, _3D, Float]
  val y = Vector.basis[Y, _3D, Float]
  val z = Vector.basis[Z, _3D, Float]

  test("SLERP should interpolate θ as expected")(pending)
/*    val p1 = Skeleton.Pose(0f, (0f, z))
    val p2 = Skeleton.Pose(2f, (Math.PI.toFloat, z))
    val p3 = Interp.slerp[Float, Float].interp(p1, p2)(1f)
    assert((p3.θ * x) === y)
  }*/

  test("SLERP should interpolate e as expected")(pending)
/*    val p1 = Skeleton.Pose(0f, (0f, x))
    val p2 = Skeleton.Pose(2f, (Math.PI.toFloat, y))
    val p3 = Interp.slerp[Float, Float].interp(p1, p2)(1f)
    val zz = Math.sqrt(2.0).toFloat / 2
    assert((p3.θ * x) === v"""0.5f 0.5f ${zz}""")
  }*/

  test("SLERP preserves continuity")(pending)

}
