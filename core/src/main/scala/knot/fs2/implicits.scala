package knot.fs2

import fs2.*

object implicits:
  extension [A](a: A)
    def pickle[F[_]](using Pickle[F, A]): Stream[F, Byte] =
      Pickle[F, A].run(a)
    def serialize[F[_], B](using Serializer[F, A, B]): Stream[F, B] =
      Serializer[F, A, B].run(a)

  extension [F[_], A](stream: Stream[F, A])
    def deserialize[B](using Deserializer[F, A, B]): F[B] =
      Deserializer[F, A, B].run(stream)

  extension [F[_]](bytes: Stream[F, Byte])
    def unpickle[A](using Unpickle[F, A]): F[A] =
      Unpickle[F, A].run(bytes)
