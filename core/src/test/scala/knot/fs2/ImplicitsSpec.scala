package knot.fs2

import implicits.*
import cats.Id
import fs2.Stream
import weaver.SimpleIOSuite

object ImplicitsSpec extends SimpleIOSuite {
  pureTest("List[Int]: serialize") {
    given Serializer[Id, List[Int], Int] =
      Serializer.instance(ls => Stream.emits(ls))
    expect.eql(
      List(1, 2, 3).serialize[Id, Int].compile.toList,
      List(1, 2, 3)
    )
  }
  pureTest("String: pickle") {
    given Pickle[Id, String] =
      Pickle.instance(s => Stream.emits(s.getBytes))
    expect.eql(
      "hello world".pickle[Id].compile.toList,
      "hello world".getBytes.toList
    )
  }
  pureTest("Stream[Id, Int]: deserialize") {
    given Deserializer[Id, Int, List[Int]] =
      Deserializer.instance(s => s.compile.toList)
    expect.eql(
      Stream(1, 2, 3).deserialize[List[Int]],
      List(1, 2, 3)
    )
  }
  pureTest("Stream[Id, Byte]: unpickle") {
    given Unpickle[Id, String] =
      Unpickle.instance(s => new String(s.compile.toList.toArray))
    expect.eql(
      Stream.emits("hello world".getBytes).unpickle[String],
      "hello world"
    )
  }
}
