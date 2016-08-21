package iliad
package syntax

import shapeless._

trait MatrixDSyntax extends MatrixContextSyntax {

  type Mat2[A] = MatrixD[nat._2, nat._2, A]
  type Mat3[A] = MatrixD[nat._3, nat._3, A]
  type Mat4[A] = MatrixD[nat._4, nat._4, A]

  type Mat2i = Mat2[Int]
  type Mat3i = Mat3[Int]
  type Mat4i = Mat4[Int]

  type Mat2f = Mat2[Float]
  type Mat3f = Mat3[Float]
  type Mat4f = Mat4[Float]

  type Mat4Algebra[A] = MatrixAlgebra[nat._4, A]

}
