package knot.fs2

import cats.{Applicative, ApplicativeThrow, Functor}
import cats.implicits.*
import cats.evidence.As
import fs2.*

/** Namespace for Unpickle monad
  *
  * Computation type: Conversion from an Stream[F,Byte] to F[A]
  *
  * Encapsulation type: Stream[F,Byte] => F[A]
  * @tparam F
  *   F-type
  * @tparam A
  *   output type
  */
trait Unpickle[F[_], A] extends Deserializer[F, Byte, A]:
  override def map[B](f: A => B)(using Functor[F]): Unpickle[F, B] =
    Unpickle.instance(run(_).map(f))

  def handleErrorWith(f: Throwable => Unpickle[F, A])(using ApplicativeThrow[F]): Unpickle[F, A] =
    Unpickle.instance(s => run(s).handleErrorWith(f(_).run(s)))

  def ap[B, C](fb: Unpickle[F, B])(using F: Applicative[F], ev: A As (B => C)): Unpickle[F, C] =
    Unpickle.instance(s => run(s).map(ev.coerce).ap(fb.run(s)))

object Unpickle:
  def apply[F[_], A](using Unpickle[F, A]): Unpickle[F, A] =
    summon[Unpickle[F, A]]

  def instance[F[_], A](f: Stream[F, Byte] => F[A]): Unpickle[F, A] =
    a => f(a)

  def pure[F[_]: Applicative, A](a: A): Unpickle[F, A] =
    instance(_ => a.pure)

  def raiseError[F[_]: ApplicativeThrow, A](e: Throwable): Unpickle[F, A] =
    instance(_ => e.raiseError)

  given [F[_]: ApplicativeThrow]: ApplicativeThrow[Unpickle[F, *]] =
    new ApplicativeThrow[Unpickle[F, *]]:
      def pure[A](a: A): Unpickle[F, A] =
        Unpickle.pure(a)

      def ap[A, B](ff: Unpickle[F, A => B])(fa: Unpickle[F, A]): Unpickle[F, B] =
        ff.ap(fa)

      def handleErrorWith[A](fa: Unpickle[F, A])(f: Throwable => Unpickle[F, A]): Unpickle[F, A] =
        fa.handleErrorWith(f)

      def raiseError[A](e: Throwable): Unpickle[F, A] =
        Unpickle.raiseError(e)
