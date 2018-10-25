package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("testToList") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("testTake") {
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
  }

  test("testDrop") {
    assert(Stream(1, 2, 3, 4, 5).drop(3).toList == List(4, 5))
  }

  test("testTakeWhile") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList == List())
    assert(Stream(2, 4, 6, 8, 11).takeWhile(_ % 2 == 0).toList == List(2, 4, 6, 8))
  }

  test("testForAll") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    assert(!Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
  }

  test("testMap") {
    assert(Stream(1, 2, 3, 4, 5).map(_ + 10).toList == List(11, 12, 13, 14, 15))
  }

  test("testFilter") {
    assert(Stream(1, 2, 3, 4, 5).filter(_ < 0).toList == List())
    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
  }

  test("testAppend") {
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  }

  test("testFlatMap") {
    assert(Stream(1, 2, 3, 4, 5).flatMap(Stream(_)).toList == List(1, 2, 3, 4, 5))
  }

}
