package iliad

import monocle._
import monocle.syntax.all._

import cats._
import cats.data._

object MonocleExtra {

  implicit def toStateTOps[F[_]: Monad, S, A](
      s: StateT[F, S, A]): StateTOps[F, S, A] =
    new StateTOps(s)

  final class StateTOps[F[_], S, A](val s: StateT[F, S, A]) extends AnyVal {
    def applyLens[R](l: Lens[R, S])(implicit M: Monad[F]): StateT[F, R, A] =
      s.transformS(_ &|-> l get, {
        case (r, s) => r &|-> l set s
      })
  }
}
