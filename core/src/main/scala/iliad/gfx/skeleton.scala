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

trait Interp[F[_, _], T, A] {
  def interp(x: F[T, A], y: F[T, A])(at: T): F[T, A]
}

/** Spherical linear interpolation */
private[gfx] trait Slerp[T, A] extends Interp[Pose, T, A] {

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
      Pose(at, (ka * (θ2.θ - θ1.θ) + θ1.θ, r * e3))
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
}

trait Skeleton[A]

object Skeleton {



  /**
    * @tparam numeric type
    * @param length bone length
    * @param θ bone orientation
    */
  case class Bone[A](length: A, θ: AxisAngle[A]) extends Skeleton[A] { self =>
    def *:(θ: AxisAngle[A])(implicit G: Ring[A], T: Trig[A]): Bone[A] = {
      //val θb = self.θ
      //Bone(length, (θb.θ, θ.matrix * θb.e))
      ???
    }
  }

  //N -> NN

  case class Joint[A](bone: Bone[A], children: SVector[Skeleton[A]]) extends Skeleton[A]
  case class IndexedPose[T, A](at: T, poses: SVector[AxisAngle[A]])
  case class Pose[T, A](at: T, θ: AxisAngle[A])

/*  case class Animation[N <: Nat, T, A](frames: List[IndexedPose[N, T, A]]) {
    def apply(skeleton: Skeleton[N, A])(t: T): Sized[SVector[Mat4[A]], N] = ???
  }*/
}


/*
import scala.{Vector => SVector}
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

  case class Joint[N <: Nat](bone: Bone[A], children: Sized[SVector[Skeleton[A]], N]) extends Skeleton[Succ[N], A]

  case class IndexedPose[N <: Nat, T, A](at: T, poses: Sized[SVector[AxisAngle[A]], N])
  case class Pose[T, A](at: T, θ: AxisAngle[A])

/*  case class Animation[N <: Nat, T, A](frames: List[IndexedPose[N, T, A]]) {
    def apply(skeleton: Skeleton[N, A])(t: T): Sized[SVector[Mat4[A]], N] = ???
  }*/
}

object TestSkeleton {
  val skeleton = Skeleton.Bone(1f, Vector,zero, Vector.zero)
  val newAnimation: Animation[nat._1, Int, Float] = ???

  val anim = newAnimation(skeleton) _
  anim(4f)
}


 */
