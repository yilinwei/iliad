package iliad

import syntax.vectord._

import shapeless._
import shapeless.nat._

import scodec._
import scodec.bits._

import Polygon._

trait Polygon[F[_], L] {
  def mapf[A, B](f: (F[A], Int) => B): Label1[L, F[A], B] = mapfi((fa, fce, idx) => f(fa, fce))
  def mapi[A, B](f: (F[A], Int) => B): Label1[L, F[A], B] = mapfi((fa, fce, idx) => f(fa, idx))
  def mapfi[A, B](f: (F[A], Int, Int) => B): Label1[L, F[A], B]
}


import scala.collection.mutable.ArrayBuilder

object Polygon {
  //Really don't want to create n different things here because we are iterating a lot!

  final class Label1[L, S, A] private[iliad] (
    generate: (S, Int, Int) => A, 
    iterate: SizeBound => ((Int, Int, ArrayBuilder[Byte]) => Option[scodec.Err]) => Attempt[BitVector], 
    n: Int) { self =>

    def map[B](f: A => B): Label1[L, S, B] = new Label1((s, fce, idx) => f(generate(s, idx, fce)), iterate, n)

    def product[B](that: Label1[L, S, B]): Label2[L, S, A, B] = ???

    def |@|[B](that: Label1[L, S, B]) = product(that)

    def encoder(implicit encoder: Encoder[A]): Encoder[S] = new Encoder[S] {
      def sizeBound = encoder.sizeBound * n
      def encode(shape: S): Attempt[BitVector] = iterate(sizeBound) { (idx, fce, builder) =>
        encoder.encode(generate(shape, idx, fce)) match {
          case Attempt.Successful(bv) => 
            builder ++= bv.bytes.toArray
            None
          case Attempt.Failure(err) => Some(err)  
        }
      }
    }
  }

  final class Label2[L, S, A0, A1] private[iliad](
    generate0: (Int, Int) => A0,
    generate1: (Int, Int) => A1,
    //TODO: BitVector
    iterate: ((Int, Int, ArrayBuilder[Byte]) => Option[scodec.Err]) => Attempt[BitVector], 
    n: Int
  ) {

    def map2[B0, B1](f0: A0 => B0, f1: A1 => B1): Label2[L, S, B0, B1] = new Label2(
      (fce, idx) => f0(generate0(idx, fce)),
      (fce, idx) => f1(generate1(idx, fce)),
      iterate,
      n
    )

    def product[A2](that: Label1[L, S, A2]): Label3[L, S, A0, A1, A2] = ???
    def |@|[A2](that: Label1[L, S, A2]): Label3[L, S, A0, A1, A2] = product(that)

    def encoder(implicit encoder0: Encoder[A0], encoder1: Encoder[A1]): Encoder[S] = new Encoder[S] {
      def sizeBound = encoder0.sizeBound * n + encoder1.sizeBound * n
      def encode(shape: S): Attempt[BitVector] = iterate { (idx, fce, builder) =>
        (for {
          bv0 <- encoder0.encode(generate0(idx, fce))
          bv1 <- encoder1.encode(generate1(idx, fce))
        } yield bv0 ++ bv1) match {
          case Attempt.Successful(bv) => 
            builder ++= bv.bytes.toArray
            None
          case Attempt.Failure(err) => Some(err)  
        }
      }
    }

  }

  trait Label3[L, S, A0, A1, A2]

}  
