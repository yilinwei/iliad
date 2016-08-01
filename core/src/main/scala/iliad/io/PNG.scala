package iliad
package io

import shapeless._

trait PNG[F <: PNG.Format]


abstract class PNGInstances {

}

object PNG {

  sealed trait Format

  trait Greyscale[N <: Nat] extends Format
  trait RGB[N <: Nat] extends Format
  trait RGBA[N <: Nat] extends Format
  trait Indexed[N <: Nat] extends Format

  //Then we can do ImageIO[PNG[PNG.Unknown].inflate[Pixels.RGBA_8888] gives the Pixel[w/e]

}



