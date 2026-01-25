package knot.fs2

import cats.*
import cats.arrow.{Arrow, Choice}
import knot.Kleisli
import fs2.Stream

/** Namespace for Serializer monad
  *
  * Computation type: Conversion from an Stream[F,A] type to a F[B] type
  *
  * Encapsulation type: Stream[F,A] => F[B]
  * @tparam F
  *   F-type
  * @tparam A
  *   input type
  * @tparam B
  *   output type
  */
trait Serializer[F[_], -A, B] extends Kleisli[Stream[F, *], A, B]:
  override def local[C](f: C => A): Serializer[F, C, B] =
    Serializer.instance(run.compose(f))

  def map[C](f: B => C): Serializer[F, A, C] =
    Serializer.instance(run(_).map(f))

  def dimap[C, D](f: C => A)(g: B => D): Serializer[F, C, D] =
    Serializer.instance(c => run(f(c)).map(g))

  def lmap[C](f: C => A): Serializer[F, C, B] =
    local(f)

  def rmap[C](f: B => C): Serializer[F, A, C] =
    map(f)

  def flatMap[C, AA <: A](f: B => Serializer[F, AA, C])(using FlatMap[F]): Serializer[F, AA, C] =
    Serializer.shift(a => run(a).flatMap(f(_).run(a)))

  def andThen[C](f: B => Stream[F, C])(using FlatMap[F]): Serializer[F, A, C] =
    Serializer.shift(a => run(a).flatMap(f))

  def andThen[C](fa: Serializer[F, B, C])(using FlatMap[F]): Serializer[F, A, C] =
    andThen(fa.run)

  def >>>[C](fa: Serializer[F, B, C])(using FlatMap[F]): Serializer[F, A, C] =
    andThen(fa.run)

  def compose[C, AA <: A](f: C => Stream[F, AA])(using FlatMap[F]): Serializer[F, C, B] =
    Serializer.shift(z => f(z).flatMap(run))

  def compose[C, AA <: A](k: Serializer[F, C, AA])(using FlatMap[F]): Serializer[F, C, B] =
    compose(k.run)

  def <<<[C, AA <: A](fa: Serializer[F, C, AA])(using FlatMap[F]): Serializer[F, C, B] =
    compose(fa.run)

  def first[C](using Functor[F]): Serializer[F, (A, C), (B, C)] =
    Serializer.instance { case (a, c) => run(a).map(b => (b, c)) }

  def second[C](using Functor[F]): Serializer[F, (C, A), (C, B)] =
    Serializer.instance { case (c, a) => run(a).map(b => (c, b)) }

  def handleErrorWith[AA <: A](f: Throwable => Serializer[F, AA, B]): Serializer[F, AA, B] =
    Serializer.instance(a => run(a).handleErrorWith(f(_).run(a)))

object Serializer extends SerializerInstances:
  def apply[F[_], A, B](using Serializer[F, A, B]): Serializer[F, A, B] =
    summon[Serializer[F, A, B]]

  def instance[F[_], A, B](f: A => Stream[F, B]): Serializer[F, A, B] =
    a => f(a)

  def shift[F[_], A, B](f: A => Stream[F, B])(using F: FlatMap[F]): Serializer[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Serializer.instance[F, A, B](a => Stream.eval(ap.pure(a)).flatMap(f))
      case _ =>
        Serializer.instance(f)
    }

  def pure[F[_], A, B](b: B): Serializer[F, A, B] =
    Serializer.instance(_ => Stream.emit(b))

  def raiseError[F[_]: ApplicativeThrow, A, B](e: Throwable): Serializer[F, A, B] =
    Serializer.instance(_ => Stream.raiseError(e))

  def ask[F[_], A]: Serializer[F, A, A] =
    Serializer.instance(a => Stream.emit(a))

sealed abstract class SerializerInstances extends SerializerInstances0:
  given [F[_]: MonadThrow, A]: MonadThrow[Serializer[F, A, *]] =
    new SerializerMonadError[F, A]:
      def F: MonadThrow[F] = MonadThrow[F]

  given [F[_]: FlatMap]: Arrow[Serializer[F, *, *]] =
    new Arrow[Serializer[F, *, *]]:
      def lift[A, B](f: A => B): Serializer[F, A, B] =
        Serializer.instance(a => Stream.emit(f(a)))

      def first[A, B, C](fa: Serializer[F, A, B]): Serializer[F, (A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: Serializer[F, B, C], g: Serializer[F, A, B]): Serializer[F, A, C] =
        f.compose(g)

  given [F[_], B]: Contravariant[Serializer[F, *, B]] =
    new Contravariant[Serializer[F, *, B]]:
      def contramap[AA, A](fa: Serializer[F, AA, B])(f: A => AA): Serializer[F, A, B] =
        fa.local(f)

sealed abstract class SerializerInstances0:
  given [F[_]: FlatMap, A]: Monad[Serializer[F, A, *]] =
    new SerializerMonad[F, A]:
      def F: FlatMap[F] = FlatMap[F]

  given [F[_]: FlatMap]: Choice[Serializer[F, *, *]] =
    new Choice[Serializer[F, *, *]]:
      def id[A]: Serializer[F, A, A] =
        Serializer.ask

      def choice[A, B, C](f: Serializer[F, A, C], g: Serializer[F, B, C]): Serializer[F, Either[A, B], C] =
        Serializer.instance(_.fold(f.run, g.run))

      def compose[A, B, C](f: Serializer[F, B, C], g: Serializer[F, A, B]): Serializer[F, A, C] =
        f.compose(g)

sealed trait SerializerMonadError[F[_], A] extends MonadThrow[Serializer[F, A, *]] with SerializerMonad[F, A]:
  implicit def F: MonadThrow[F]

  def handleErrorWith[B](fa: Serializer[F, A, B])(f: Throwable => Serializer[F, A, B]): Serializer[F, A, B] =
    fa.handleErrorWith(f)

  def raiseError[B](e: Throwable): Serializer[F, A, B] =
    Serializer.raiseError(e)

sealed trait SerializerMonad[F[_], A] extends Monad[Serializer[F, A, *]]:
  implicit def F: FlatMap[F]

  def pure[B](b: B): Serializer[F, A, B] =
    Serializer.pure(b)

  def flatMap[B, C](fa: Serializer[F, A, B])(f: B => Serializer[F, A, C]): Serializer[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Serializer[F, A, Either[B, C]]): Serializer[F, A, C] =
    Serializer.instance { a =>
      Monad[Stream[F, *]].tailRecM(b)(f(_).run(a))
    }
