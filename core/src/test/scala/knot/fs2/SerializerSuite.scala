package knot.fs2

import cats.*
import cats.effect.IO
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import fs2.Stream
import knot.fs2.util.CatsEffectInstances.given
import knot.fs2.util.Fs2Instances.given
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object SerializerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[A => Stream[F, B]]): Eq[Serializer[F, A, B]] =
    Eq.by[Serializer[F, A, B], A => Stream[F, B]](_.run)

  given [F[_], A, B](using Arbitrary[A => Stream[F, B]]): Arbitrary[Serializer[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => Stream[F, B]].map(Serializer.instance))

  checkAll("Serializer[IO, MiniInt, *]", MonadErrorTests[Serializer[IO, MiniInt, *], Throwable].monadError[Int, Int, Int])
  checkAll("Serializer[Id, *, *]", ArrowTests[Serializer[Id, *, *]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Serializer[Id, *, *]", ChoiceTests[Serializer[Id, *, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Serializer[Id, MiniInt, *]", MonadTests[Serializer[Id, MiniInt, *]].monad[Int, Int, Int])
  checkAll("Serializer[Id, *, Int]", ContravariantTests[Serializer[Id, *, Int]].contravariant[MiniInt, Int, Boolean])
  pureTest("map") {
    val fa = Serializer
      .instance[Id, List[Int], Int](Stream.emits)
      .map(_ + 1)
    expect.eql(fa.run(List(1, 2, 3)), Stream(2, 3, 4))
  }
  pureTest("dimap") {
    val fa = Serializer
      .instance[Id, List[Int], Int](Stream.emits)
      .dimap[Int, String](i => List.fill(i)(1))(_.toString)
    expect.eql(fa.run(3), Stream("1", "1", "1"))
  }
  pureTest("lmap") {
    val fa = Serializer
      .instance[Id, List[Int], Int](Stream.emits)
      .lmap[Int](i => List.fill(i)(1))
    expect.eql(fa.run(3), Stream(1, 1, 1))
  }
  pureTest("rmap") {
    val fa = Serializer
      .instance[Id, List[Int], Int](Stream.emits)
      .rmap(_ + 1)
    expect.eql(fa.run(List(1, 2, 3)), Stream(2, 3, 4))
  }
  pureTest("andThen") {
    val fa = Serializer.instance[Id, List[Int], Int](Stream.emits)
    val fb = Serializer.instance[Id, Int, String](i => Stream(i.toString))
    val fc = fa.andThen(fb.run)
    val fd = fa.andThen(fb)
    val fe = fa >>> fb
    expect.eql(fc.run(List(1, 2, 3)), Stream("1", "2", "3")) and
      expect.eql(fd.run(List(1, 2, 3)), Stream("1", "2", "3")) and
      expect.eql(fe.run(List(1, 2, 3)), Stream("1", "2", "3"))
  }
  pureTest("compose") {
    val fa = Serializer.instance[Id, Int, String](i => Stream(i.toString))
    val fb = Serializer.instance[Id, List[Int], Int](Stream.emits)
    val fc = fa <<< fb
    expect.eql(fc.run(List(1, 2, 3)), Stream("1", "2", "3"))
  }
  pureTest("second") {
    val fa = Serializer
      .instance[Id, List[Int], Int](Stream.emits)
      .second[Char]
    expect.eql(fa.run('c' -> List(1, 2, 3)), Stream('c' -> 1, 'c' -> 2, 'c' -> 3))
  }

  object ImplicitResolution:
    given Serializer[Id, List[Int], Int] =
      Serializer.instance[Id, List[Int], Int](Stream.emits)

    Serializer[Id, List[Int], Int]
}
