package knot.fs2.circe

import cats.implicits.*
import cats.{Applicative, ApplicativeThrow, Functor}
import cats.effect.Concurrent
import cats.evidence.As
import io.circe.Decoder
import knot.fs2.Unpickle
import fs2.*
import fs2.data.json._
import fs2.data.json.circe._

/** Namespace for Json unpickle
  *
  * Computation type: Conversion from an Stream[F,Byte] to F[A] using json
  *
  * Encapsulation type: Stream[F,Byte] => F[A]
  * @tparam F
  *   F-type
  * @tparam A
  *   output type
  */
trait JsonUnpickle[F[_], A] extends Unpickle[F, A]:
  override def map[B](f: A => B)(using Functor[F]): JsonUnpickle[F, B] =
    JsonUnpickle.instance(run(_).map(f))

  def handleErrorWith(f: Throwable => JsonUnpickle[F, A])(using ApplicativeThrow[F]): JsonUnpickle[F, A] =
    JsonUnpickle.instance(s => run(s).handleErrorWith(f(_).run(s)))

  def orElse(fa: JsonUnpickle[F, A])(using ApplicativeThrow[F]): JsonUnpickle[F, A] =
    handleErrorWith(_ => fa)

  def ap[B, C](fb: JsonUnpickle[F, B])(using F: Applicative[F], ev: A As (B => C)): JsonUnpickle[F, C] =
    JsonUnpickle.instance(s => run(s).map(ev.coerce).ap(fb.run(s)))

object JsonUnpickle:
  def apply[F[_], A](using JsonUnpickle[F, A]): JsonUnpickle[F, A] =
    summon[JsonUnpickle[F, A]]

  def instance[F[_], A](f: Stream[F, Byte] => F[A]): JsonUnpickle[F, A] =
    m => f(m)

  def liftF[F[_], A](fa: F[A]): JsonUnpickle[F, A] =
    instance(_ => fa)

  def pure[F[_]: Applicative, A](a: A): JsonUnpickle[F, A] =
    liftF(a.pure[F])

  def raiseError[F[_]: ApplicativeThrow, A](e: Throwable): JsonUnpickle[F, A] =
    liftF(e.raiseError[F, A])

  /** Unpickle using standard json syntax
    * @tparam F
    *   effect type
    * @tparam A
    *   output type
    */
  def json[F[_]: Concurrent, A: Decoder]: JsonUnpickle[F, A] =
    instance { s =>
      s
        .through(fs2.text.utf8.decode)
        .through(ast.parse)
        .compile
        .lastOrError
        .flatMap(_.as.liftTo)
    }

  /** Unpickle using standard yaml syntax
    *
    * @tparam F
    *   effect type
    * @tparam A
    *   output type
    */
  def yaml[F[_]: Concurrent, A: Decoder]: JsonUnpickle[F, A] =
    instance { s =>
      s
        .through(text.utf8.decode)
        .compile
        .string
        .flatMap(s => io.circe.yaml.parser.decode(s).liftTo)
    }

  /** Unpickle using a superset of json syntax (json, yaml)
    *
    * @tparam F
    *   effect type
    * @tparam A
    *   output type
    */
  def superset[F[_]: Concurrent, A: Decoder]: JsonUnpickle[F, A] =
    json.orElse(yaml)

  given [F[_]: ApplicativeThrow]: ApplicativeThrow[JsonUnpickle[F, *]] =
    new ApplicativeThrow[JsonUnpickle[F, *]]:
      def pure[A](a: A): JsonUnpickle[F, A] =
        JsonUnpickle.pure(a)

      def ap[A, B](ff: JsonUnpickle[F, A => B])(fa: JsonUnpickle[F, A]): JsonUnpickle[F, B] =
        ff.ap(fa)

      def handleErrorWith[A](fa: JsonUnpickle[F, A])(f: Throwable => JsonUnpickle[F, A]): JsonUnpickle[F, A] =
        fa.handleErrorWith(f)

      def raiseError[A](e: Throwable): JsonUnpickle[F, A] =
        JsonUnpickle.raiseError(e)
