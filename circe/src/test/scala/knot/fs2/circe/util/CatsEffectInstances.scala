package knot.fs2.circe.util

import cats.Eq
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalacheck.Arbitrary

object CatsEffectInstances {
  given [A: Eq]: Eq[IO[A]] =
    Eq.instance { case (a, b) =>
      Eq[Either[Throwable, A]].eqv(a.attempt.unsafeRunSync(), b.attempt.unsafeRunSync())
    }

  given Eq[Throwable] =
    Eq.fromUniversalEquals

  given [A: Arbitrary]: Arbitrary[IO[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(IO.pure))
}
