package knot.fs2.circe

import cats.effect.Concurrent
import fs2.*
import io.circe.Decoder

object implicits:
  extension [F[_]: Concurrent](bytes: Stream[F, Byte])
    def unpickleJson[A: Decoder]: F[A] =
      JsonUnpickle.json[F, A].run(bytes)

    def unpickleYaml[A: Decoder]: F[A] =
      JsonUnpickle.yaml[F, A].run(bytes)

    def unpickleJsonSuperset[A: Decoder]: F[A] =
      JsonUnpickle.superset[F, A].run(bytes)
