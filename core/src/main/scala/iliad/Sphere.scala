package iliad

import spire.math._
import spire.algebra._
import spire.implicits._

import iliad.syntax.vectord._

/** cartesian sphere */
case class Sphere[A](r: Vec3[A], dr: A) {
  def origin: Vec3[A] = r

  def bounded(v: Vec3[A])(implicit N: Numeric[A]): Boolean = {
    val r0 = r - v
    (r0 ⋅ r0) < (dr * dr)
  }

  def translate(t: Vec3[A])(implicit N: Numeric[A]): Sphere[A] =
    this.copy(r = this.r + t)

  def rotate(r: Vec2[A])(implicit N: Numeric[A], T: Trig[A]): Sphere[A] = ???

  def cmap[B]: Sphere[A] = ???
}

object Sphere {
  //TODO: cache Vector[A] for Int and Float and Double
  def apply[A](r: A): Sphere[A] = ???
}
