package knot.fs2.circe

import cats.effect.IO
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.{Eq, Id}
import fs2.*
import io.circe.Json
import io.circe.syntax.*
import io.circe.yaml.syntax.*
import knot.fs2.circe.util.CatsEffectInstances.given
import knot.fs2.circe.util.Fs2Instances.given
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object JsonUnpickleSuite extends SimpleIOSuite with Discipline {
  given [F[_], A](using Eq[Stream[F, Byte] => F[A]]): Eq[JsonUnpickle[F, A]] =
    Eq.by[JsonUnpickle[F, A], Stream[F, Byte] => F[A]](_.run)

  given [F[_], A](using Cogen[Stream[F, Byte]], Arbitrary[F[A]]): Arbitrary[JsonUnpickle[F, A]] =
    Arbitrary(Arbitrary.arbitrary[Stream[F, Byte] => F[A]].map(JsonUnpickle.instance))

  checkAll("JsonUnpickle[IO, *]", ApplicativeErrorTests[JsonUnpickle[IO, *], Throwable].applicativeError[Int, Int, Int])

  pureTest("JsonUnpickle[Id, Int]: map") {
    val fa = JsonUnpickle
      .instance[Id, Int](s => s.compile.fold(0)(_ + _))
      .map(_.toString)
    expect.eql(fa.run(Stream(0.toByte, 1.toByte, 2.toByte)), "3")
  }
  test("JsonUnpickle[IO, Int]: orElse") {
    val fa   = JsonUnpickle.json[IO, Json]
    val fb   = JsonUnpickle.pure(Json.Null)
    val fc   = fa.orElse(fb)
    val json = Json.obj("foo" -> "bar".asJson)
    for {
      r1 <- fc.run(Stream.emits(json.noSpaces.getBytes))
      r2 <- fc.run(Stream.emits("{".getBytes))
    } yield expect.eql(r1, json) and expect.eql(r2, Json.Null)
  }
  test("JsonUnpickle: json") {
    val fa = JsonUnpickle.json[IO, Map[String, String]]
    for {
      r1 <- fa.run(
        Stream.emits(
          Json.obj("foo" -> "bar".asJson).noSpaces.getBytes
        )
      )
      r2 <- fa.run(Stream.emits("{".getBytes)).attempt
    } yield expect.eql(r1, Map("foo" -> "bar")) and expect(r2.isLeft)
  }
  test("JsonUnpickle: yaml") {
    val fa = JsonUnpickle.yaml[IO, Map[String, String]]
    for {
      r1 <- fa.run(
        Stream.emits(
          Json.obj("foo" -> "bar".asJson).asYaml.spaces2.getBytes
        )
      )
      r2 <- fa.run(Stream.emits("{".getBytes)).attempt
    } yield expect.eql(r1, Map("foo" -> "bar")) and expect(r2.isLeft)
  }
  test("JsonUnpickle: superset") {
    val fa = JsonUnpickle.superset[IO, Map[String, String]]
    for {
      r1 <- fa.run(
        Stream.emits(
          Json.obj("foo" -> "bar".asJson).noSpaces.getBytes
        )
      )
      r2 <- fa.run(
        Stream.emits(
          Json.obj("foo" -> "bar".asJson).asYaml.spaces2.getBytes
        )
      )
      r3 <- fa.run(Stream.emits("{".getBytes)).attempt
    } yield expect.eql(r1, Map("foo" -> "bar")) and
      expect.eql(r2, Map("foo" -> "bar")) and
      expect(r3.isLeft)
  }

  object ImplicitResolution:
    given JsonUnpickle[Id, String] =
      JsonUnpickle.instance(s => new String(s.compile.toList.toArray))

    JsonUnpickle[Id, String]
}
