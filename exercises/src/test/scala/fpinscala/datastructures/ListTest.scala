package fpinscala.datastructures

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  import fpinscala.datastructures.List._

  test("testRes") {
    assert(res == 3)
  }

  test("testSetHead") {
    assert(setHead(Nil, 10) == List(10))
    assert(setHead(List(1, 2, 3, 4, 5), 10) == List(10, 2, 3, 4, 5))
  }

  test("testDrop") {
    assert(drop(Nil, 2) == Nil)
    assert(drop(List(1, 2, 3, 4, 5), 4) == List(5))
  }

  test("testTail") {
    assert(tail(Nil) == Nil)
    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  test("testDropWhile") {
    assert(dropWhile(Nil)(_ => true) == Nil)
    assert(dropWhile(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(1, 3, 5))
  }

  test("testInit") {
    assert(init(Nil) == Nil)
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  test("testLength") {
    assert(length(Nil) == 0)
    assert(length(List(1)) == 1)
    assert(length(List(1, 2, 3, 4, 5)) == 5)
  }

  test("testSum") {
    assert(sum(Nil) == 0)
    assert(sum(List(1, 2, 3, 4, 5)) == 15)
  }

  test("testSum2") {
    assert(sum2(Nil) == 0)
    assert(sum2(List(1, 2, 3, 4, 5)) == 15)
  }

  test("testSum3") {
    assert(sum3(Nil) == 0)
    assert(sum3(List(1, 2, 3, 4, 5)) == 15)
  }

  test("testProduct") {
    assert(product(Nil) == 1)
    assert(product(List(1, 2, 3, 4, 5)) == 120)
  }

  test("testProduct2") {
    assert(product2(Nil) == 1)
    assert(product2(List(1, 2, 3, 4, 5)) == 120)
  }

  test("testProduct3") {
    assert(product3(Nil) == 1)
    assert(product3(List(1, 2, 3, 4, 5)) == 120)
  }

  test("testLength2") {
    assert(length2(Nil) == 0)
    assert(length2(List(1)) == 1)
    assert(length2(List(1, 2, 3, 4, 5)) == 5)
  }

  test("testReverse") {
    assert(reverse(Nil) == Nil)
    assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

}
