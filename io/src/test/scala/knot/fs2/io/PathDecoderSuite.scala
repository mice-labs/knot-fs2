package knot.fs2.io

import cats.Eq
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import fs2.io.file.Path
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object PathDecoderSuite extends SimpleIOSuite with Discipline {
  given ExhaustiveCheck[Path] =
    ExhaustiveCheck.instance(
      List(
        Path("/home/user/documents/test.csv"),
        Path("C:\\Program Files\\test.csv")
      )
    )

  given Cogen[Path] =
    Cogen[String].contramap(p => p.toString)

  given [E, A](using Eq[Path => Either[E, A]]): Eq[PathDecoder[E, A]] =
    Eq.by[PathDecoder[E, A], Path => Either[E, A]](_.run)

  given [E, A](using Arbitrary[E], Arbitrary[A]): Arbitrary[PathDecoder[E, A]] =
    Arbitrary(Arbitrary.arbitrary[Path => Either[E, A]].map(PathDecoder.instance))

  checkAll("PathDecoder[MiniInt, *]", MonadErrorTests[PathDecoder[MiniInt, *], MiniInt].monadError[Int, Int, Int])

  pureTest("PathDecoder[String, String]: map") {
    val fa = PathDecoder
      .instance(p =>
        p.extName match {
          case ""      => "File does not have an extension".asLeft
          case extName => extName.asRight
        }
      )
      .map(_.toUpperCase)
    expect.eql(fa.run(Path("hello.json")), "JSON".asRight) and
      expect(fa.run(Path(".gitignore")).isLeft)
  }

  object ImplicitResolution:
    given PathDecoder[String, String] =
      PathDecoder.instance(p =>
        p.extName match {
          case ""      => "File does not have an extension".asLeft
          case extName => extName.asRight
        }
      )
    PathDecoder[String, String]
}
