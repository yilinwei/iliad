package iliad
package io
package ops

import java.io.InputStream
import java.nio.ByteBuffer

object file {
  trait FileRead[A] {
    def byteSize: Int
    def decode(arr: Array[Byte]): A
  }

  final class IntFileRead extends FileRead[Int] {
    val byteSize = 4
    def decode(arr: Array[Byte]): Int = ByteBuffer.wrap(arr).getInt()
  }

  final class FloatFileRead extends FileRead[Float] {
    val byteSize = 4
    def decode(arr: Array[Byte]): Float = ByteBuffer.wrap(arr).getFloat()
  }


  abstract class FileReadInstances {
    implicit val intFileRead: FileRead[Int] = new IntFileRead
    implicit val floatFileRead: FileRead[Float] = new FloatFileRead
  }

  object FileRead extends FileReadInstances {
  }

  trait FileOpen {
    def open(file: File.Descriptor): InputStream
  }
}
