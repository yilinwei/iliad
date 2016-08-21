package iliad
package ops

object shape {
  trait Bound[F[_], G[_], A]

  object Bound {
    def identityBound[F[_], A]: Bound[F, F, A] = ???
  }
}
