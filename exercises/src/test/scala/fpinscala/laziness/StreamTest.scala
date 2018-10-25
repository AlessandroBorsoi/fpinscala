package fpinscala.laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  import fpinscala.laziness.Stream._

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

  test("testOnes") {
    assert(ones.map(_ + 1).exists(_ % 2 == 0))
    assert(!ones.forAll(_ != 1))
    assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("testConstant") {
    assert(constant(1).map(_ + 1).exists(_ % 2 == 0))
    assert(!constant(1).forAll(_ != 1))
    assert(constant(1).take(5).toList == List(1, 1, 1, 1, 1))
    assert(constant(2).map(_ * 2).exists(_ % 2 == 0))
    assert(!constant(2).forAll(_ != 2))
    assert(constant(2).take(5).toList == List(2, 2, 2, 2, 2))
  }

  test("testFrom") {
    assert(from(5).take(4).toList == List(5, 6, 7, 8))
    assert(from(22).map(_ * 7).filter(_ % 3 == 0).take(2).toList == List(168, 189))
  }

  test("testFibs") {
    assert(fibs().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("testOnesUnfold") {
    assert(onesUnfold.map(_ + 1).exists(_ % 2 == 0))
    assert(!onesUnfold.forAll(_ != 1))
    assert(onesUnfold.take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("testConstantUnfold") {
    assert(constantUnfold(1).map(_ + 1).exists(_ % 2 == 0))
    assert(!constantUnfold(1).forAll(_ != 1))
    assert(constantUnfold(1).take(5).toList == List(1, 1, 1, 1, 1))
    assert(constantUnfold(2).map(_ * 2).exists(_ % 2 == 0))
    assert(!constantUnfold(2).forAll(_ != 2))
    assert(constantUnfold(2).take(5).toList == List(2, 2, 2, 2, 2))
  }

  test("testFromUnfold") {
    assert(fromUnfold(5).take(4).toList == List(5, 6, 7, 8))
    assert(fromUnfold(22).map(_ * 7).filter(_ % 3 == 0).take(2).toList == List(168, 189))
  }

  test("testFibsUnfold") {
    assert(fibsUnfold().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("testMapUnfold") {
    assert(Stream(1, 2, 3, 4, 5).mapUnfold(_ + 10).toList == List(11, 12, 13, 14, 15))
  }

  test("testTakeUnfold") {
    assert(Stream(1, 2, 3, 4, 5).takeUnfold(3).toList == List(1, 2, 3))
  }

  test("testTakeWhileUnfold") {
    assert(Stream(1, 2, 3, 4, 5).takeWhileUnfold(_ % 2 == 0).toList == List())
    assert(Stream(2, 4, 6, 8, 11).takeWhileUnfold(_ % 2 == 0).toList == List(2, 4, 6, 8))
  }

}
