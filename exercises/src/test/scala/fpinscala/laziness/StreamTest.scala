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

}
