package iliad
package algebra

import iliad.algebra.syntax.vector._

import monocle.std.tuple2._

import spire.math._
import spire.algebra._
import spire.algebra.Sign._
import spire.implicits._

import shapeless._

import syntax.matrix._
import syntax.axisAngle._

import AxisAngle._

final class AxisAngleOps[A](val value: AxisAngle[A]) extends AnyVal {
  def e = value._2
  def θ = value._1

  def matrix(implicit G: Ring[A], T: Trig[A]): OMat3[A] = {
    val one = G.one
    val zero = G.zero
    val c0 = T.cos(θ)
    val c1 = one - c0
    val c2 = T.sin(θ)
    val x = e.x
    val y = e.y
    val z = e.z
    ortho"""${c0 + x * x * c1}   ${x * y * c1 - z * c2} ${x * z * c1 + y * c2}
            ${x * y * c1 + z * c2} ${c0 + y * y * c1}   ${y * z * c1 - x * c2}
            ${x * z * c1 - y * c2} ${z * y * c1 + x * c2} ${c0 + z * z * c1}"""
  }

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): AxisAngle[A] = 
    _θ[A].modify(a * _)(value)

  def /:(a: A)(implicit G: MultiplicativeGroup[A]): AxisAngle[A] = 
    _θ[A].modify(_ / a)(value)

  def *(v: Vec3[A])(implicit G0: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A], G1: Ring[A], T: Trig[A]): Vec3[A] =
    matrix * v

  def unary_-(implicit G: Ring[A]): AxisAngle[A] = _θ[A].modify(-_)(value)
}

object AxisAngle {
  def _e[A] = tuple2Field2[A, Vec3[A]].second
  def _θ[A] = tuple2Field1[A, Vec3[A]].first

  def apply[A](θ: A, e: Vec3[A]): AxisAngle[A] = (θ, e)
  def apply[A](ortho: OMat3[A])(implicit F: Fractional[A], T: Trig[A], G: Ring[A]): AxisAngle[A] = {
    val m = ortho.matrix
    val two = F.fromInt(2)
    val cθ = (m.trace - F.one / two)
    if(cθ >= F.fromInt(1))
      (F.zero, Vector.basis[Z, _3D, A])
    else {
      val θ = if (cθ <= F.fromInt(-1)) T.pi else T.acos(cθ)      
      val scale = F.zero / (two * T.sin(θ))
      val x = scale *: (m(1, 2) - m(2, 1))
      val y = scale *: (m(2, 0) - m(0, 2))
      val z = scale *: (m(0, 1) - m(1, 0))
      apply(θ, v"$x $y $z")
    }
  }

  def zero[A](implicit R: Ring[A]): AxisAngle[A] = (R.zero, Vector.basis[Z, _3D, A])

  def rotation[A](x: Vec3[A], y: Vec3[A])(implicit G: Ring[A], E: Eq[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A]): AxisAngle[A] = {
    val a = x × y
    val n = a.norm
    val e = if (n === G.zero) Vector.basis[Z, _3D, A] else a :/ n
    val θ = T.acos(x ⋅ y)
    (θ, e)
  }

  def rotation[A](x: Vec2[A], y: Vec2[A])(implicit G: Ring[A], E: Eq[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A], ev: DummyImplicit): AxisAngle[A] = {
    val z = G.zero
    rotation(x :+ z, y :+ z)
  }

  def basisRotation[A](x1: Vec3[A], y1: Vec3[A])(x2: Vec3[A], y2: Vec3[A])(implicit F: Fractional[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A], G0: MultiplicativeSemigroup[Mat3[A]]): AxisAngle[A] = {
    val r1 = (-rotationToBasis(x1, y1)).matrix
    val r2 = rotationToBasis(x2, y2).matrix
    AxisAngle(r2 * r1)
  }

  //TODO: Call this basisRotation
  def rotationToBasis[A](x: Vec3[A], y: Vec3[A])(implicit F: Fractional[A], 
    T: Trig[A],
    N: NormedVectorSpace[Vec3[A], A]): AxisAngle[A] = {
    def approxOne(v: A): Boolean = v > F.fromInt(0) && v < F.fromInt(2)
    val z = x × y
    //calculate a rotation matrix;
    //[a b c
    // d e f
    // g h i]
    val a = x.x
    val d = x.y
    val g = x.z
    val b = y.x
    val e = y.y
    val h = y.z
    val c = z.x
    val f = z.y
    val i = z.z

    val u = v"${h - f} ${c - g} ${d - b}"

    val axis = u.normalize
    if(approxOne(axis.norm)) {
      val sinθ = u.norm / F.fromInt(2)
      val cosθ = (a + e + i - F.one) / F.fromInt(2)
      val θ = T.atan(sinθ / cosθ)
      if(cosθ.sign == Negative) (θ + T.pi, axis)
      else (θ, axis)
    }
    else if(approxOne(a) && approxOne(e) && approxOne(i)) zero[A]
    else {
      //rotation of PI
      val axis = if(a > -F.one) {
        val ex = F.sqrt((a + F.one) / F.fromInt(2))
        val ey = b / (F.fromInt(2) * ex)
        val ez = c / (F.fromInt(2) * ex)
        v"$ex $ey $ez".normalize
      } else if(e > -F.one) {
        val ey = F.sqrt((e + F.one) / F.fromInt(2))
        val ex = b / (F.fromInt(2) * ey)
        val ez = f / (F.fromInt(2) * ey)
        v"$ex $ey $ez".normalize
      } else Vector.basis[Z, _3D, A]     

      (T.pi, axis)
    }
  }
}
