package iliad

import simulacrum.typeclass

@typeclass
trait SizeOf[A] {
  def byteSize: Int
}

import java.nio.ByteBuffer

trait ByteOps[A] {
  def byteSize: Int
  def get(bb: ByteBuffer): A
  def put(bb: ByteBuffer, a: A): Unit
  def apply(a: A): ByteBuffer = {
    val bb = ByteBuffer.allocate(byteSize)
    put(bb, a)
    bb
  }
}

object ByteIso {
  implicit val intByteOps: ByteOps[Int] = new IntByteOps
  implicit val FloatByteOps: ByteOps[Float] = new FloatByteOps
}

private final class IntByteOps extends ByteOps[Int] {
  val byteSize = 4
  def get(bb: ByteBuffer): Int = bb.getInt()
  def put(bb: ByteBuffer, a: Int): Unit = bb.putInt(a)
}

private final class FloatByteOps extends ByteOps[Float] {
  val byteSize = 4
  def get(bb: ByteBuffer): Float = bb.getFloat()
  def put(bb: ByteBuffer, a: Float): Unit= bb.putFloat(a)
}
