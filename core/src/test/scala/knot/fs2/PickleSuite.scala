package knot.fs2

import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.{Eq, Id}
import fs2.Stream
import knot.fs2.util.Fs2Instances.given
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object PickleSuite extends SimpleIOSuite with Discipline {
  given [F[_], A](using Eq[A => Stream[F, Byte]]): Eq[Pickle[F, A]] =
    Eq.by[Pickle[F, A], A => Stream[F, Byte]](_.run)

  given [F[_], A](using Arbitrary[A], Cogen[A]): Arbitrary[Pickle[F, A]] =
    Arbitrary(Arbitrary.arbitrary[A => Stream[F, Byte]].map(Pickle.instance))

  checkAll("Pickle[Id, *]", ContravariantTests[Pickle[Id, *]].contravariant[MiniInt, Int, Boolean])
  pureTest("Pickle[Id, String]: contramap") {
    val fa = Pickle
      .instance[Id, String](s => Stream.emits(s.getBytes))
      .contramap[Long](l => l.toString)
    expect.eql(fa.run(123L), Stream.emits("123".getBytes))
  }
  object ImplicitResolution:
    given Pickle[Id, String] =
      Pickle.instance[Id, String](s => Stream.emits(s.getBytes))
    Pickle[Id, String]
}
