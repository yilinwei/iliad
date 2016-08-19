package iliad

import spire.math._
import spire.algebra._
import spire.implicits._

import iliad.syntax.vectord._

/** Cartesian cuboid */
case class Cuboid[A](
  o: Vec3[A],
  xn: Vec3[A],
  yn: Vec3[A],
  zn: Vec3[A],
  dx: A,
  dy: A,
  dz: A
) {

  def origin(implicit N: Numeric[A]): Vec3[A] = (dx *: xn + dy *: yn + dz *: zn) + o

  def cmap[B]: Cuboid[B] = ???

  def bounded(v: Vec3[A])(implicit N: Numeric[A]): Boolean =
    ((o - v) ⋅ xn < (dx * dx)) && ((o - v) ⋅ yn < (dy * dy))

  def translate(t: Vec3[A])(implicit N: Numeric[A]): Cuboid[A] =
    this.copy(o = t + this.o)

  def rotate(r: Vec2[A])(implicit N: Numeric[A], T: Trig[A]): Cuboid[A] = ???

}

object Cuboid {
  def apply[A](dx: A, dy: A, dz: A): Cuboid[A] = ???
}




