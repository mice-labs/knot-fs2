package knot.fs2.circe

import cats.effect.IO
import implicits.*
import fs2.*
import io.circe.Json
import io.circe.syntax.*
import io.circe.yaml.syntax.*
import weaver.SimpleIOSuite

object ImplicitsSuite extends SimpleIOSuite {
  test("Stream[IO, Byte]: unpickleJson") {
    for {
      r1 <- Stream
        .emits(
          Json.obj("foo" -> "bar".asJson).noSpaces.getBytes
        )
        .unpickleJson[Map[String, String]]
    } yield expect.eql(r1, Map("foo" -> "bar"))
  }
  test("Stream[IO, Byte]: unpickleYaml") {
    val fa = JsonUnpickle.yaml[IO, Map[String, String]]
    for {
      r1 <- Stream
        .emits(
          Json.obj("foo" -> "bar".asJson).asYaml.spaces2.getBytes
        )
        .unpickleYaml[Map[String, String]]
    } yield expect.eql(r1, Map("foo" -> "bar"))
  }
  test("Stream[IO, Byte]: unpickleJsonSuperset") {
    for {
      r1 <- Stream
        .emits(
          Json.obj("foo" -> "bar".asJson).noSpaces.getBytes
        )
        .unpickleJsonSuperset[Map[String, String]]
      r2 <- Stream
        .emits(
          Json.obj("foo" -> "bar".asJson).asYaml.spaces2.getBytes
        )
        .unpickleJsonSuperset[Map[String, String]]
    } yield expect.eql(r1, Map("foo" -> "bar")) and
      expect.eql(r2, Map("foo" -> "bar"))
  }
}
