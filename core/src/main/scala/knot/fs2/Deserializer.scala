package knot.fs2

import cats.*
import cats.evidence.As
import cats.implicits.*
import fs2.*
import knot.Kleisli
import knot.fs2.*

/** Namespace for Deserializer monad
  *
  * Computation type: Conversion from an A type to a Stream[F, B] type
  *
  * Encapsulation type: A => Stream[F,B]
  * @tparam F
  *   F-type
  * @tparam A
  *   input type
  * @tparam B
  *   output type
  */
trait Deserializer[F[_], A, B] extends Kleisli[F, Stream[F, A], B]:
  override def map[C](f: B => C)(using Functor[F]): Deserializer[F, A, C] =
    Deserializer.instance(run(_).map(f))

  def local[C](f: C => A): Deserializer[F, C, B] =
    Deserializer.instance(s => run(s.map(f)))

  def contramap[C](f: C => A): Deserializer[F, C, B] =
    local(f)

  def dimap[C, D](f: C => A)(g: B => D)(using Functor[F]): Deserializer[F, C, D] =
    Deserializer.instance(s => run(s.map(f)).map(g))

  def lmap[C](f: C => A): Deserializer[F, C, B] =
    contramap(f)

  override def rmap[C](f: B => C)(using Functor[F]): Deserializer[F, A, C] =
    map(f)

  def ap[C, D, AA <: A](f: Deserializer[F, AA, C])(using F: Apply[F], ev: B As (C => D)): Deserializer[F, AA, D] = {
    Deserializer.instance { a =>
      val fb: F[C => D] = F.map(run(a))(ev.coerce)
      val fc: F[C]      = f.run(a)
      F.ap(fb)(fc)
    }
  }

  def flatMap[C](f: B => Deserializer[F, A, C])(using FlatMap[F]): Deserializer[F, A, C] =
    Deserializer.shift(a => run(a).flatMap(f(_).run(a)))

object Deserializer extends DeserializerInstances:
  def apply[F[_], A, B](using Deserializer[F, A, B]): Deserializer[F, A, B] =
    summon[Deserializer[F, A, B]]

  def instance[F[_], A, B](f: Stream[F, A] => F[B]): Deserializer[F, A, B] =
    a => f(a)

  def unit[F[_], A](using InvariantMonoidal[F]): Deserializer[F, A, Unit] =
    Deserializer.instance(_ => InvariantMonoidal[F].unit)

  def shift[F[_], A, B](f: Stream[F, A] => F[B])(using F: FlatMap[F]): Deserializer[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Deserializer.instance(r => F.flatMap(ap.pure(r))(f))
      case _ =>
        Deserializer.instance(f)
    }

  def pure[F[_]: Applicative, A, B](b: B): Deserializer[F, A, B] =
    instance(_ => b.pure[F])

  def liftF[F[_], A, B](fb: F[B]): Deserializer[F, A, B] =
    instance(_ => fb)

sealed abstract class DeserializerInstances extends DeserializerInstances0:
  given [A]: CommutativeMonad[Deserializer[Id, A, *]] =
    commutativeMonadForDeserializer[Id, A]

sealed abstract class DeserializerInstances0 extends DeserializerInstances1:
  given commutativeMonadForDeserializer[F[_], A](using CommutativeMonad[F]): CommutativeMonad[Deserializer[F, A, *]] =
    new DeserializerMonad[F, A] with CommutativeMonad[Deserializer[F, A, *]]:
      def F: Monad[F] = Monad[F]

sealed abstract class DeserializerInstances1 extends DeserializerInstances2:
  given [F[_], A, B](using Monoid[F[B]]): Monoid[Deserializer[F, A, B]] =
    new DeserializerMonoid[F, A, B]:
      def FB: Monoid[F[B]] = Monoid[F[B]]

  given [F[_], A, E](using MonadError[F, E]): MonadError[Deserializer[F, A, *], E] =
    new DeserializerMonadError[F, A, E]:
      def F: MonadError[F, E] = MonadError[F, E]

  given [F[_]: ContravariantMonoidal, A]: ContravariantMonoidal[Deserializer[F, A, *]] =
    new DeserializerContravariantMonoidal[F, A]:
      def F: ContravariantMonoidal[F] = ContravariantMonoidal[F]

sealed abstract class DeserializerInstances2 extends DeserializerInstances3:
  given monadForDeserializer[F[_]: Monad, A]: Monad[Deserializer[F, A, *]] =
    new DeserializerMonad[F, A]:
      def F: Monad[F] = Monad[F]

  given [F[_], B]: Contravariant[Deserializer[F, *, B]] =
    new Contravariant[Deserializer[F, *, B]]:
      override def contramap[AA, A](fa: Deserializer[F, AA, B])(f: A => AA): Deserializer[F, A, B] =
        fa.local(f)

sealed abstract class DeserializerInstances3 extends DeserializerInstances4:
  given [F[_]: Alternative, A]: Alternative[Deserializer[F, A, *]] =
    new DeserializerAlternative[F, A]:
      def F: Alternative[F] = Alternative[F]

sealed abstract class DeserializerInstances4 extends DeserializerInstances5:
  given [F[_]: MonoidK, A]: MonoidK[Deserializer[F, A, *]] =
    new DeserializerMonoidK[F, A]:
      def F: MonoidK[F] = MonoidK[F]

  given commutativeFlatMapForDeserializer[F[_]: CommutativeFlatMap, A]: CommutativeFlatMap[Deserializer[F, A, *]] =
    new DeserializerFlatMap[F, A] with CommutativeFlatMap[Deserializer[F, A, *]]:
      def F: CommutativeFlatMap[F] = CommutativeFlatMap[F]

  given semigroupForDeserializer[F[_], A, B](using Semigroup[F[B]]): Semigroup[Deserializer[F, A, B]] =
    new DeserializerSemigroup[F, A, B]:
      def FB: Semigroup[F[B]] = Semigroup[F[B]]

sealed abstract class DeserializerInstances5 extends DeserializerInstances6:
  given [F[_]: SemigroupK, A]: SemigroupK[Deserializer[F, A, *]] =
    new DeserializerSemigroupK[F, A]:
      def F: SemigroupK[F] = SemigroupK[F]

  given flatMapForDeserializer[F[_]: FlatMap, A]: FlatMap[Deserializer[F, A, *]] =
    new DeserializerFlatMap[F, A]:
      def F: FlatMap[F] = FlatMap[F]

sealed abstract class DeserializerInstances6 extends DeserializerInstances7:
  given applicativeErrorForDeserializer[F[_], A, E](using ApplicativeError[F, E]): ApplicativeError[Deserializer[F, A, *], E] =
    new DeserializerApplicativeError[F, A, E]:
      def F: ApplicativeError[F, E] = ApplicativeError[F, E]

sealed abstract class DeserializerInstances7 extends DeserializerInstances8:
  given applicativeForDeserializer[F[_]: Applicative, A]: Applicative[Deserializer[F, A, *]] =
    new DeserializerApplicative[F, A]:
      def F: Applicative[F] = Applicative[F]

sealed abstract class DeserializerInstances8 extends DeserializerInstances9:
  given applyForDeserializer[F[_]: Apply, A]: Apply[Deserializer[F, A, *]] =
    new DeserializerApply[F, A]:
      def F: Apply[F] = Apply[F]

sealed abstract class DeserializerInstances9:
  given functorForDeserializer[F[_]: Functor, A]: Functor[Deserializer[F, A, *]] =
    new DeserializerFunctor[F, A]:
      def F: Functor[F] = Functor[F]

private trait DeserializerMonadError[F[_], A, E] extends MonadError[Deserializer[F, A, *], E] with DeserializerApplicativeError[F, A, E] with DeserializerMonad[F, A]:
  def F: MonadError[F, E]

private trait DeserializerMonad[F[_], A] extends Monad[Deserializer[F, A, *]] with DeserializerFlatMap[F, A] with DeserializerApplicative[F, A]:
  implicit def F: Monad[F]

private trait DeserializerFlatMap[F[_], A] extends FlatMap[Deserializer[F, A, *]] with DeserializerApply[F, A]:
  implicit def F: FlatMap[F]

  def flatMap[B, C](fa: Deserializer[F, A, B])(f: B => Deserializer[F, A, C]): Deserializer[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Deserializer[F, A, Either[B, C]]): Deserializer[F, A, C] =
    Deserializer.instance { s =>
      F.tailRecM(b)(f(_).run(s))
    }

private trait DeserializerApplicativeError[F[_], A, E] extends ApplicativeError[Deserializer[F, A, *], E] with DeserializerApplicative[F, A]:
  implicit def F: ApplicativeError[F, E]

  def raiseError[B](e: E): Deserializer[F, A, B] =
    Deserializer.instance(_ => F.raiseError(e))

  def handleErrorWith[B](fb: Deserializer[F, A, B])(f: E => Deserializer[F, A, B]): Deserializer[F, A, B] =
    Deserializer.instance { s =>
      F.handleErrorWith(fb.run(s))(e => f(e).run(s))
    }

private trait DeserializerApplicative[F[_], A] extends Applicative[Deserializer[F, A, *]] with DeserializerApply[F, A]:
  implicit def F: Applicative[F]

  def pure[B](x: B): Deserializer[F, A, B] =
    Deserializer.pure[F, A, B](x)

private trait DeserializerApply[F[_], A] extends Apply[Deserializer[F, A, *]] with DeserializerFunctor[F, A]:
  implicit def F: Apply[F]

  override def ap[AA, B](ff: Deserializer[F, A, AA => B])(fa: Deserializer[F, A, AA]): Deserializer[F, A, B] =
    ff.ap(fa)

  override def map2Eval[B, C, D](fa: Deserializer[F, A, B], fb: Eval[Deserializer[F, A, C]])(
      f: (B, C) => D
  ): Eval[Deserializer[F, A, D]] = {
    // We should only evaluate fb once
    val memoFb = fb.memoize

    Eval.now(Deserializer.instance { s =>
      val fb              = fa.run(s)
      val efc             = memoFb.map(_.run(s))
      val efz: Eval[F[D]] = F.map2Eval(fb, efc)(f)
      // This is not safe and results in stack overflows:
      // see: https://github.com/typelevel/cats/issues/3947
      efz.value
    })
  }

  override def product[B, C](fb: Deserializer[F, A, B], fc: Deserializer[F, A, C]): Deserializer[F, A, (B, C)] =
    Deserializer.instance(a => Apply[F].product(fb.run(a), fc.run(a)))

private trait DeserializerFunctor[F[_], A] extends Functor[Deserializer[F, A, *]]:
  implicit def F: Functor[F]

  override def map[B, C](fa: Deserializer[F, A, B])(f: B => C): Deserializer[F, A, C] =
    fa.map(f)

  override def void[B](fa: Deserializer[F, A, B]): Deserializer[F, A, Unit] =
    Deserializer.instance(fa.run.andThen(Functor[F].void))

private trait DeserializerMonoid[F[_], A, B] extends Monoid[Deserializer[F, A, B]] with DeserializerSemigroup[F, A, B]:
  implicit def FB: Monoid[F[B]]

  override def empty: Deserializer[F, A, B] = Deserializer.instance(_ => FB.empty)

private trait DeserializerSemigroup[F[_], A, B] extends Semigroup[Deserializer[F, A, B]]:
  implicit def FB: Semigroup[F[B]]

  override def combine(a: Deserializer[F, A, B], b: Deserializer[F, A, B]): Deserializer[F, A, B] =
    Deserializer.instance(x => FB.combine(a.run(x), b.run(x)))

private trait DeserializerContravariantMonoidal[F[_], A] extends ContravariantMonoidal[Deserializer[F, A, *]]:
  implicit def F: ContravariantMonoidal[F]

  def unit: Deserializer[F, A, Unit] =
    Deserializer.unit

  override def contramap[B, C](fa: Deserializer[F, A, B])(f: C => B): Deserializer[F, A, C] =
    Deserializer.instance(s => F.contramap(fa.run(s))(f))

  override def product[B, C](fa: Deserializer[F, A, B], fb: Deserializer[F, A, C]): Deserializer[F, A, (B, C)] =
    Deserializer.instance(s => F.product(fa.run(s), fb.run(s)))

private trait DeserializerAlternative[F[_], A] extends Alternative[Deserializer[F, A, *]] with DeserializerApplicative[F, A] with DeserializerMonoidK[F, A]:
  implicit def F: Alternative[F]

private trait DeserializerMonoidK[F[_], A] extends MonoidK[Deserializer[F, A, *]] with DeserializerSemigroupK[F, A]:
  implicit def F: MonoidK[F]

  override def empty[B]: Deserializer[F, A, B] = Deserializer.liftF(F.empty[B])

private trait DeserializerSemigroupK[F[_], A] extends SemigroupK[Deserializer[F, A, *]]:
  implicit def F: SemigroupK[F]

  override def combineK[B](x: Deserializer[F, A, B], y: Deserializer[F, A, B]): Deserializer[F, A, B] =
    Deserializer.instance(a => F.combineK(x.run(a), y.run(a)))

  override def combineKEval[B](x: Deserializer[F, A, B], y: Eval[Deserializer[F, A, B]]): Eval[Deserializer[F, A, B]] =
    Eval.now(Deserializer.instance(a => F.combineKEval(x.run(a), y.map(_.run(a))).value))
