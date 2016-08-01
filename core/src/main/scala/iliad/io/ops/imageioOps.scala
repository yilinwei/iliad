package iliad
package io
package ops

import shapeless._
import cats.data._

import Image._

object image {
  trait InflateChunk[A, L <: HList] {
    def inflate(data: Chunk[Byte], width: Int, height: Int, red: Int, blue: Int, green: Int, alpha: Int): Throwable Xor Pixels[L]
  }

  trait CompressChunk[A] {
    def compress(data: Chunk[Byte], width: Int, height: Int, red: Int, blue: Int, green: Int, alpha: Int): Throwable Xor Chunk[Byte]
  }
}
