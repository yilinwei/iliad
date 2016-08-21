package iliad

import simulacrum._
import shapeless._

import spire.math._


import iliad.ops.shape._
import shapeless.ops.nat._

trait Shape[N <: Nat, F[_], A] {

  def origin(fa: F[A]): VectorD[N, A]
  def bounded(fa: F[A])(v: VectorD[N, A]): Boolean
  def translate(fa: F[A])(t: VectorD[N, A]): F[A]  //N - 1
  def rotate[NN <: Nat](fa: F[A])(r: VectorD[NN, A])(implicit ev: Pred.Aux[N, NN]): F[A]
  def convert[B : ConvertableTo](fa: F[A])(implicit F: ConvertableFrom[A]): F[B]

}
