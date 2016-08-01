package iliad
package io

import java.io.{InputStream, IOException, FileNotFoundException}

import cats.data._
import cats.implicits._

sealed trait File

object File {

  //TODO: Think about OutputStream
  type Stream = InputStream

  case class Tree(nodes: List[File]) extends File
  case class Descriptor(name: String) extends File

  trait Dir {
    def open(file: Descriptor): IOException Xor Stream
    def list: List[File]
  }

  object Dir {
    def apply(implicit dir: Dir) = dir

    def from(fs: Map[Descriptor, InputStream]): Dir = new Dir {
      def open(file: Descriptor): IOException Xor Stream = fs.get(file) match {
        case Some(is) => is.right
        case None => new FileNotFoundException(s"unable to find file $file in $fs").left
      }
      def list: List[File] = fs.keys.toList
    }
    def from(fs: (Descriptor, InputStream)*): Dir = from(Map(fs: _*))
  }
}
