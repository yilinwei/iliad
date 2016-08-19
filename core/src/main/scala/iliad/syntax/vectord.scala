package iliad
package syntax

import iliad.std.all._
import iliad.kernel._

import shapeless._
import shapeless.ops.nat._

import shapeless._

trait VectorDSyntax extends VectorContextSyntax {

  type Vec2[A] = VectorD[nat._2, A]
  type Vec3[A] = VectorD[nat._3, A]
  type Vec4[A] = VectorD[nat._4, A]

  type Vec2i = Vec2[Int]
  type Vec3i = Vec3[Int]
  type Vec4i = Vec4[Int]

  type Vec2f = Vec2[Float]
  type Vec3f = Vec3[Float]
  type Vec4f = Vec4[Float]
}
