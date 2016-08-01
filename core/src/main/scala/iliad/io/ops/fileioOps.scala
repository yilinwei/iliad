package iliad
package io
package ops

import java.nio.ByteBuffer

object file {
  trait FileRead[A] {
    def byteSize: Int
    def decode(arr: Array[Byte]): A
  }

  final class IntFileRead extends FileRead[Int] {
    val byteSize = 4
    def decode(arr: Array[Byte]): Int = {
      println(arr)
      ByteBuffer.wrap(arr).getInt()
    }
  }

  abstract class FileReadInstances {
    implicit val intFileRead: FileRead[Int] = new IntFileRead
  }

  object FileRead extends FileReadInstances {
  }
}
