package iliad

import spire.algebra._
import spire.math._
import spire.implicits._

import iliad.syntax.vectord._

/** cartesian cuboid */
case class Cube[A](
  o: Vec3[A],
  xn: Vec3[A],
  yn: Vec3[A],
  zn: Vec3[A],
  w: A
) {

  def origin(implicit N: Numeric[A]): Vec3[A] = (w *: xn + w *: yn + w *: zn) + o

  def convert[B : ConvertableTo](implicit F: ConvertableFrom[A]): Cube[B] = Cube(
    o.map(F.toType[B]),
    xn.map(F.toType[B]),
    yn.map(F.toType[B]),
    zn.map(F.toType[B]),
    F.toType[B](w)
  )
 
  def bounded(v: Vec3[A])(implicit N: Numeric[A]): Boolean = {
    val ww = w * w
    ((o - v) ⋅ xn < ww) && ((o - v) ⋅ yn < ww)
  }

  def translate(t: Vec3[A])(implicit N: Numeric[A]): Cube[A] =
    this.copy(o = t + this.o)

  //TODO: use cross product
  def rotate(r: Vec2[A])(implicit N: Numeric[A], T: Trig[A]): Cube[A] = ???

}

object Cube extends CubeInstances {
  // TODO: Import Field
  def apply[A](w: A)(implicit V: InnerProductSpace[Vec3[A], A], N: Numeric[A]): Cube[A] = {
    val o = N.fromInt(1)
    val z = N.zero
    Cube(
      V.zero,
      v"$o $z $z",
      v"$z $o $z",
      v"$o $o $z",
      w
    )
  }
}

import shapeless._

//TODO: cube algebra cache Int and Float
abstract class CubeInstances {
  implicit def cubeIsShape[A](implicit N0: Numeric[A], T0: Trig[A]): Shape[nat._3, Cube, A] =
    new CubeIsShape[A] {
      val N = N0
      val T = T0
    }
}


import shapeless.ops.nat._

private[iliad] sealed trait CubeIsShape[A] extends Shape[nat._3, Cube, A] {

  implicit val N: Numeric[A]
  implicit val T: Trig[A]

  def origin(c: Cube[A]): Vec3[A] = c.origin
  def bounded(c: Cube[A])(v: Vec3[A]): Boolean = c.bounded(v)
  def translate(c: Cube[A])(t: Vec3[A]): Cube[A] = c.translate(t)
  def rotate[NN <: Nat](c: Cube[A])(r: VectorD[NN, A])(implicit ev: Pred.Aux[nat._3, NN]): Cube[A] =
    c.rotate(r.asInstanceOf)

  def convert[B : ConvertableTo](c: Cube[A])(implicit F: ConvertableFrom[A]): Cube[B] =
    c.convert[B]
}

