package iliad
package io

import shapeless._

object Image {

  trait R
  trait G
  trait B
  trait A

  type RGB_565 = (R,  nat._5) :: (G, nat._6) :: (B, nat._5) :: HNil
  type RGBA_8888 = (R, nat._8) :: (G, nat._8) :: (B, nat._8) :: (A, nat._8) :: HNil
  type A_8 = (A, nat._8) :: HNil

  case class Pixels[L <: HList](width: Int, height: Int, data: Chunk[Byte])
  case class Compressed[A](data: Chunk[Byte])
}
