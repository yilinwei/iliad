package iliad
package gfx

import scala.{Vector => SVector}

import iliad.algebra._
import iliad.algebra.syntax.axisAngle._

import spire._
import spire.implicits._
import spire.math._
import spire.algebra._

import shapeless._

import Skeleton._

/*trait Interp[F[_, _], T, A] {
  def interp(x: F[T, A], y: F[T, A])(at: T): F[T, A]
}*/

/** Spherical linear interpolation */
/*private[gfx] trait Slerp[T, A] extends Interp[Pose, T, A] {

  implicit def G0: Ring[A]
  implicit def G1: Module[Vec3[A], A]
  implicit def G2: Field[T]
  implicit def G3: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]

  implicit def C0: ConvertableFrom[T]
  implicit def EQ: Eq[A]
  implicit def T: Trig[A]

  def C1: ConvertableTo[A]

  def interp(x: Pose[T, A], y: Pose[T, A])(at: T): Pose[T, A] = {
    val Pose(t1, θ1) = x
    val Pose(t2, θ2) = y
    val kt = (at - t1) / (t2 - t1)
    val ka = C1.fromType[T](kt)
    val e3 = θ1.e × θ2.e
    if(e3 === Vector.zero[A](3)) {
      val sign = if(θ1.e === θ2.e) G0.one else - G0.one
      val dθ = sign * θ2.θ - θ1.θ      
      Pose(at, (ka * dθ + θ1.θ, θ1.e))
    } else {
      val ϕ = T.acos(θ1.e ⋅ θ2.e)
      val r = (ka * ϕ, e3)
      Pose(at, ((ka + G0.one) * θ1.θ, r * θ1.e))
    }
  }
}

object Interp {
  def slerp[T : Field: ConvertableFrom, A: Ring : Eq : Trig : ConvertableTo](implicit M: Module[Vec3[A], A], G: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): Interp[Pose, T, A] = new Slerp[T, A] {
    val G0 = Ring[A]
    val G1 = M
    val G2 = Field[T]
    val G3 = G
    val C0 = ConvertableFrom[T]
    val EQ = Eq[A]
    val T = Trig[A]
    val C1 = ConvertableTo[A]
  }
}*/

object Interp {
  type AnimM[T, A] = T => SVector[Mat4[A]]
}

trait Interp[T, A] {
  def interpM(t1: T, s1: Skeleton[A])(t2: T, s2: Skeleton[A]): Interp.AnimM[T, A]
}

private[gfx] final class Slerp[T : Order, A: Trig : Fractional : Eq](
  implicit N: NormedVectorSpace[Vec3[A], A],
  G0: MultiplicativeSemigroup[Mat3[A]],
  G1: MultiplicativeSemigroup[Mat4[A]]) extends Interp[T, A] {

//  private def interpM(xt: T, yt: T, ts: SVector[Mat4[]]

  def interpM(xt: T, x: Skeleton[A])(yt: T, y: Skeleton[A]): Interp.AnimM[T, A] = {
    x.foldIndexed2(y, List.empty[(Int, AxisAngle[A])]) { (b, idx, xx, yy) =>
      (idx, AxisAngle.basisRotation(xx.x, xx.y)(yy.x, yy.y)) :: b
    } match {
      case Some(axisAngles) => 
        val paths = x.pathByIndex
        val lengths = y.lengthByIndex
        val size = x.size

        t => {

          val id = G1.zero

          paths.map { p =>
            p.foldLeft(Matrix)
          }

          axisAngles(0).matrix
          //we want to do a matrix transform

          ???
        }
      case None => throw new IllegalArgumentException("trying to interpolate between skeletons of a different structure!")  
    }
  }
}


trait Skeleton[A] { self =>
  
  def foldLeft[B](b: B)(f: (B, Bone[A]) => B): B = {
    def go(acc: B, s: Skeleton[A]): B = s match {
      case b: Bone[A] => f(acc, b)
      case _ @ Joint(p, bs) => bs.foldLeft(f(acc, p))((b, a) => go(b, a))
    }
    go(b, self)
  }

  def pathByIndex: Map[Int, List[Int]] = {
    val ps = foldPath(List.empty[(Int, List[Int])])((b, i, p) => (i, p) :: b)
    ps.toMap
  }

  def lengthByIndex: Map[Int, A] = {
    val ls = foldIndexed(List.empty[(Int, A)])((ls, i, b) => (i, b.length) :: ls)
    ls.toMap
  }

  def foldPath[B](b: B)(f: (B, Int, List[Int]) => B): B = 
    foldIndexedWithPath[B](b)((acc, idx, _, p) => f(acc, idx, p))

  def foldIndexedWithPath[B](b: B)(f: (B, Int, Bone[A], List[Int]) => B): B = {
    def go(acc: B, idx: Int, s: Skeleton[A], path: List[Int]): (B, Int) = s match {
      case b: Bone[A] =>
        (f(acc, idx, b, path), idx)
      case _  @ Joint(p, bs) =>
        val next = f(acc, idx, p, path)
        val path2 = idx :: path
        bs.foldLeft((next, idx)) { (bb, aa) =>
          go(bb._1, bb._2 + 1, aa, path2)
        }
    }
    go(b, 0, self, List.empty)._1
  }

  def foldIndexed[B](b: B)(f: (B, Int, Bone[A]) => B): B = 
    foldIndexedWithPath(b)((acc, idx, b, _) => f(acc, idx, b))

  def size: Int = foldLeft(0)((s, _) => s + 1)

  def foldLeft2[B](that: Skeleton[A], b: B)(f: (B, Bone[A], Bone[A]) => B): Option[B] = {
    def go(acc: Option[B], x: Skeleton[A], y: Skeleton[A]): Option[B] = {
      acc.flatMap { bb =>
        (x, y) match {
          case (xx: Bone[A], yy: Bone[A]) => Some(f(bb, xx, yy))
          case (_ @ Joint(xx, bs1), _ @ Joint(yy, bs2)) =>
            if(bs1.size != bs2.size) None else bs1.zip(bs2).foldLeft(Option(f(acc.get, xx, yy))) { (bbb, ab) =>
              go(bbb, ab._1, ab._2)
            }
          case (_: Bone[A], _) => None
          case (_, _: Bone[A]) => None
        }
      }
    }
    go(Some(b), self, that)
  }

  def foldIndexed2[B](that: Skeleton[A], b: B)(f: (B, Int, Bone[A], Bone[A]) => B): Option[B] = {
    foldLeft2(that, (b, 0)) { (acc, ba, bb) =>
      val idx = acc._2
      (f(acc._1, idx, ba, bb), idx + 1)
    }.map(_._1)      
  }

}

object Skeleton {

  type Animation[A, T] = List[KeyFrame[A, T]]
  type Children[A] = SVector[A]


  //x, y, z form the basis vector for the bone, l is always in the direction of z

  case class Bone[A](length: A, x: Vec3[A], y: Vec3[A]) extends Skeleton[A] {
    def z: Vec3[A] = ???
  }
  case class Joint[A](parent: Bone[A], children: Children[Skeleton[A]]) extends Skeleton[A]
  val Children = SVector
  
  case class KeyFrame[A, T](at: T, skeleton: Skeleton[A])





  /**
    * @tparam numeric type
    * @param length bone length
    * @param θ bone orientation
    */
/*  case class Bone[A](length: A, θ: AxisAngle[A]) extends Skeleton[A] { self =>
    def *:(θ: AxisAngle[A])(implicit G: Ring[A], T: Trig[A]): Bone[A] = {
      //val θb = self.θ
      //Bone(length, (θb.θ, θ.matrix * θb.e))
      ???
    }
  }

 

  //N -> NN

  case class Joint[A](bone: Bone[A], children: Childen[Skeleton[A]]) extends Skeleton[A]
  case class IndexedPose[T, A](at: T, poses: Childen[AxisAngle[A]])*/
  case class Pose[T, A](at: T, θ: AxisAngle[A]) 

/*  case class Animation[N <: Nat, T, A](frames: List[IndexedPose[N, T, A]]) {
    def apply(skeleton: Skeleton[N, A])(t: T): Sized[Childen[Mat4[A]], N] = ???
  }*/
}


/*
import scala.{Vector => Childen}
import spire.algebra._

trait Render[A] {
  def render(a: A): List[Gfx]
}

import shapeless._
import iliad.algebra._

import iliad.algebra.syntax.matrix._


import Skeleton._

/**
  * @tparam T numeric time type
  * @tparam A numeric bone type
  */




private[gfx] trait IndexedPoseInterp[N <: Nat, T, A] extends Interp[IndexedPose[N, ?, ?], T, A] {
  def I: Interp[Pose[T, A]]
  def interp(x: IndexedPose[N, T, A], y: IndexedPose[N, T, A]): IndexedPose[N, T, A] = {
    Sized.wrap(x.poses.unsized.zip(y.poses.unsized).map {
      case (θx, θy) => I.interp(Pose(x.at, θx), Pose(y.at, θy))
    })
  }
}

/**
  * @tparam N number of bones
  * @tparam A numeric type
  */
trait Skeleton[N <: Nat, A] { self =>

}

object Skeleton {



  /**
    * @tparam numeric type
    * @param length bone length
    * @param θ bone orientation
    */
  case class Bone[A](length: A, θ: AxisAngle[A]) extends Skeleton[nat._1, A] { self =>
    def *:(θ: AxisAngle[A])(implicit G: Ring[A], T: Trig[A]): Bone[A] = {
      val θb = self.θ
      Bone(length, (θb.θ, θ.matrix * θb.e))
    }
  }

  //N -> NN

  case class Joint[N <: Nat](bone: Bone[A], children: Sized[Childen[Skeleton[A]], N]) extends Skeleton[Succ[N], A]

  case class IndexedPose[N <: Nat, T, A](at: T, poses: Sized[Childen[AxisAngle[A]], N])
  case class Pose[T, A](at: T, θ: AxisAngle[A])

/*  case class Animation[N <: Nat, T, A](frames: List[IndexedPose[N, T, A]]) {
    def apply(skeleton: Skeleton[N, A])(t: T): Sized[Childen[Mat4[A]], N] = ???
  }*/
}

object TestSkeleton {
  val skeleton = Skeleton.Bone(1f, Vector,zero, Vector.zero)
  val newAnimation: Animation[nat._1, Int, Float] = ???

  val anim = newAnimation(skeleton) _
  anim(4f)
}


 */
