package knot.fs2

import cats.*
import cats.effect.IO
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.kernel.laws.discipline.*
import fs2.*
import knot.fs2.util.CatsEffectInstances.given
import knot.fs2.util.Fs2Instances.given
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

import scala.util.Try

object DeserializerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[Stream[F, A] => F[B]]): Eq[Deserializer[F, A, B]] =
    Eq.by[Deserializer[F, A, B], Stream[F, A] => F[B]](_.run)

  given [F[_], A, B](using Arbitrary[Stream[F, A] => F[B]]): Arbitrary[Deserializer[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[Stream[F, A] => F[B]].map(Deserializer.instance))

  checkAll("Deserializer[Id, MiniInt, *]", CommutativeMonadTests[Deserializer[Id, MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, Int]", MonoidTests[Deserializer[Id, MiniInt, Int]].monoid)
  checkAll("Deserializer[IO, MiniInt, *]", MonadErrorTests[Deserializer[IO, MiniInt, *], Throwable].monadError[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", MonadTests[Deserializer[Id, MiniInt, *]](Deserializer.monadForDeserializer).monad[Int, Int, Int])
  checkAll("Deserializer[Id, *, Int]", ContravariantTests[Deserializer[Id, *, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll(
    "Deserializer[Id, MiniInt, *]",
    CommutativeFlatMapTests[Deserializer[Id, MiniInt, *]](Deserializer.commutativeFlatMapForDeserializer).commutativeFlatMap[MiniInt, Int, Boolean]
  )
  checkAll("Deserializer[Id, MiniInt, Int]", SemigroupTests[Deserializer[Id, MiniInt, Int]](Deserializer.semigroupForDeserializer).semigroup)
  checkAll("Deserializer[Id, MiniInt, Int]", FlatMapTests[Deserializer[Id, MiniInt, *]](Deserializer.flatMapForDeserializer).flatMap[Int, Int, Int])
  checkAll(
    "Deserializer[IO, MiniInt, *]",
    ApplicativeErrorTests[Deserializer[IO, MiniInt, *], Throwable](Deserializer.applicativeErrorForDeserializer).applicativeError[Int, Int, Int]
  )
  checkAll("Deserializer[Id, MiniInt, *]", ApplicativeTests[Deserializer[Id, MiniInt, *]](Deserializer.applicativeForDeserializer).applicative[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", ApplyTests[Deserializer[Id, MiniInt, *]](Deserializer.applyForDeserializer).apply[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", FunctorTests[Deserializer[Id, MiniInt, *]](Deserializer.functorForDeserializer).functor[Int, Int, Int])
  pureTest("Deserializer[Id, Int, Int]: contramap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .contramap[Boolean](b => if (b) 1 else 0)
    expect.eql(fa.run(Stream(true, false, true)), 2)
  }
  pureTest("Deserializer[Id, Int, Int]: dimap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .dimap[Boolean, String](b => if (b) 1 else 0)(_.toString)
    expect.eql(fa.run(Stream(true, false, true)), "2")
  }
  pureTest("Deserializer[Id, Int, Int]: lmap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .lmap[Boolean](b => if (b) 1 else 0)
    expect.eql(fa.run(Stream(true, false, true)), 2)
  }
  pureTest("Deserializer[Id, Int, Int]: rmap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .rmap(_.toString)
    expect.eql(fa.run(Stream(1, 2, 3)), "6")
  }
  pureTest("Deserializer: unit") {
    val fa = Deserializer.unit[Id, Int]
    expect.eql(fa.run(Stream(1)), ())
  }
  pureTest("Deserializer: liftF") {
    val fa = Deserializer.liftF[Id, String, Int](1)
    expect.eql(fa.run(Stream("3")), 1)
  }
  pureTest("Deserializer: liftF") {
    val fa = Deserializer.liftF[Id, String, Int](1)
    expect.eql(fa.run(Stream("3")), 1)
  }

  object ImplicitResolution:
    Monoid[Deserializer[Option, MiniInt, Int]]
    Monad[Deserializer[Option, MiniInt, *]]
    MonoidK[Deserializer[List, MiniInt, *]]
    Alternative[Deserializer[Option, MiniInt, *]]
    CommutativeFlatMap[Deserializer[Option, MiniInt, *]]
    Semigroup[Deserializer[Option, MiniInt, Int]]
    SemigroupK[Deserializer[Option, MiniInt, *]]
    FlatMap[Deserializer[Option, MiniInt, *]]
    ApplicativeError[Deserializer[Either[Throwable, *], MiniInt, *], Throwable]
    Apply[Deserializer[Option, MiniInt, *]]
    Functor[Deserializer[Option, MiniInt, *]]
}
