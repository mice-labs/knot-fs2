package knot.fs2

import cats.Contravariant
import knot.data.Serializer
import fs2.*

/** Namespace for Pickle monad
  *
  * Computation type: Conversion from an A type to a Stream[F,Byte]
  *
  * Encapsulation type: A => Stream[F,Byte]
  * @tparam F
  *   F-type
  * @tparam A
  *   input type
  */
trait Pickle[F[_], A] extends Serializer[F, A, Byte]:
  override def local[B](f: B => A): Pickle[F, B] =
    Pickle.instance(b => run(f(b)))

  def contramap[B](f: B => A): Pickle[F, B] =
    local(f)

object Pickle:
  def apply[F[_], A](using Pickle[F, A]): Pickle[F, A] =
    summon[Pickle[F, A]]

  def instance[F[_], A](f: A => Stream[F, Byte]): Pickle[F, A] =
    a => f(a)

  given [F[_]]: Contravariant[Pickle[F, *]] =
    new Contravariant[Pickle[F, *]]:
      def contramap[AA, A](fa: Pickle[F, AA])(f: A => AA): Pickle[F, A] =
        fa.local(f)
