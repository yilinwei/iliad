package iliad
package io

import cats.data._
import shapeless._

import Image._

trait ImageIO[A] {

  import ops.image._

  def inflate[L <: HList](compressed: Compressed[A])(implicit inflate: InflateChunk[A, L]): Throwable Xor Pixels[L] =
    //TODO: A should tell me what I expect!
    inflate.inflate(compressed.data, 1, 1, 8, 8, 8, 8)

  def compress[L <: HList](pixels: Pixels[L])(implicit compress: CompressChunk[A]): Throwable Xor Compressed[A] = {
    val Pixels(w, h, d) = pixels
    //TODO: Pixels create size of each component!
    compress.compress(d, w, h, 8, 8, 8, 8).map(Compressed(_))
  }

}

object ImageIO {
  def apply[A](implicit imageIO: ImageIO[A]): ImageIO[A] = imageIO
}
