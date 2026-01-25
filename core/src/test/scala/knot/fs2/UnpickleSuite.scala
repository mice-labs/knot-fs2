package knot.fs2

import cats.effect.IO
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.{Eq, Id}
import fs2.Stream
import knot.fs2.util.CatsEffectInstances.given
import knot.fs2.util.Fs2Instances.given
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object UnpickleSuite extends SimpleIOSuite with Discipline {
  given ExhaustiveCheck[Byte] =
    ExhaustiveCheck.instance((Byte.MinValue to Byte.MaxValue).map(_.toByte).toList)

  given [F[_], A](using Eq[Stream[F, Byte] => F[A]]): Eq[Unpickle[F, A]] =
    Eq.by[Unpickle[F, A], Stream[F, Byte] => F[A]](_.run)

  given [F[_], A](using Cogen[Stream[F, Byte]], Arbitrary[F[A]]): Arbitrary[Unpickle[F, A]] =
    Arbitrary(Arbitrary.arbitrary[Stream[F, Byte] => F[A]].map(Unpickle.instance))

  checkAll("Unpickle[IO, MiniInt, *]", ApplicativeErrorTests[Unpickle[IO, *], Throwable].applicativeError[Int, Int, Int])
  pureTest("Unpickle[Int, Int]: map") {
    val fa = Unpickle
      .instance[Id, Int](s => s.compile.fold(0)(_ + _))
      .map(_.toString)
    expect.eql(fa.run(Stream(0.toByte, 1.toByte, 2.toByte)), "3")
  }

  object ImplicitResolution:
    given Unpickle[Id, String] =
      Unpickle.instance(s => new String(s.compile.toList.toArray))
    Unpickle[Id, String]
}
