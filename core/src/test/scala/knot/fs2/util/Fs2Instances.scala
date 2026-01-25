package knot.fs2.util

import cats.{Eq, Id}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.laws.discipline.ExhaustiveCheck
import fs2.{Compiler, Stream}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Fs2Instances {
  val STREAM_LENGTH = 3

  given [F[_], A: ExhaustiveCheck]: ExhaustiveCheck[Stream[F, A]] =
    ExhaustiveCheck.instance {
      List(Stream.empty, Stream.emits(ExhaustiveCheck[A].allValues.take(STREAM_LENGTH)))
    }

  given [F[_], G[_], A](using Compiler[F, G], Eq[G[List[A]]]): Eq[Stream[F, A]] =
    Eq.by(_.take(STREAM_LENGTH).compile.toList)

  given [F[_], A: Arbitrary]: Arbitrary[Stream[F, A]] =
    Arbitrary(Gen.listOfN(STREAM_LENGTH, Arbitrary.arbitrary[A]).map(Stream.emits))

  given cogenIOStream[A](using Cogen[A]): Cogen[Stream[IO, A]] =
    Cogen[List[A]].contramap { s =>
      s.take(STREAM_LENGTH).compile.toList.unsafeRunSync()
    }

  given cogenIdStream[A](using Cogen[A]): Cogen[Stream[Id, A]] =
    Cogen[List[A]].contramap { s =>
      s.take(STREAM_LENGTH).compile.toList
    }
}
