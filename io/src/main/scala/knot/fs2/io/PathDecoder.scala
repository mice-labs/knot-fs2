package knot.fs2.io

import cats.{Monad, MonadError}
import cats.evidence.As
import cats.implicits.*
import fs2.io.file.Path
import knot.Decoder

trait PathDecoder[E, A] extends Decoder[E, Path, A]:
  override def map[B](f: A => B): PathDecoder[E, B] =
    PathDecoder.instance(run(_).map(f))

  def flatMap[B](f: A => PathDecoder[E, B]): PathDecoder[E, B] =
    PathDecoder.instance(uri => run(uri).flatMap(f(_).run(uri)))

  def handleErrorWith(f: E => PathDecoder[E, A]): PathDecoder[E, A] =
    PathDecoder.instance(uri => run(uri).handleErrorWith(e => f(e).run(uri)))

object PathDecoder:
  def apply[E, A](using PathDecoder[E, A]): PathDecoder[E, A] =
    summon[PathDecoder[E, A]]

  def instance[E, A](f: Path => Either[E, A]): PathDecoder[E, A] =
    uri => f(uri)

  def pure[E, A](a: A): PathDecoder[E, A] =
    instance(_ => a.asRight)

  def raiseError[E, A](e: E): PathDecoder[E, A] =
    instance(_ => e.asLeft)

  given [E]: MonadError[PathDecoder[E, *], E] =
    new MonadError[PathDecoder[E, *], E]:
      def pure[A](a: A): PathDecoder[E, A] =
        PathDecoder.pure(a)

      def raiseError[A](e: E): PathDecoder[E, A] =
        PathDecoder.raiseError(e)

      def handleErrorWith[A](fa: PathDecoder[E, A])(f: E => PathDecoder[E, A]): PathDecoder[E, A] =
        fa.handleErrorWith(f)

      def flatMap[A, B](fa: PathDecoder[E, A])(f: A => PathDecoder[E, B]): PathDecoder[E, B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => PathDecoder[E, Either[A, B]]): PathDecoder[E, B] =
        PathDecoder.instance { json =>
          Monad[Either[E, *]].tailRecM(a)(a => f(a).run(json))
        }
