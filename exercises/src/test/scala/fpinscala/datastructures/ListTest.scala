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

  test("testAppen1") {
    assert(append1(Nil, Nil) == Nil)
    assert(append1(List(1, 2, 3), Nil) == List(1, 2, 3))
    assert(append1(Nil, List(1, 2, 3)) == List(1, 2, 3))
    assert(append1(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  test("testConcat") {
    assert(concat(Nil) == Nil)
    assert(concat(List(Nil)) == Nil)
    assert(concat(List(List(1))) == List(1))
    assert(concat(List(List(1), List(2, 3), List(4), List(5, 6))) == List(1, 2, 3, 4, 5, 6))
  }

  test("testIncr") {
    assert(incr(Nil) == Nil)
    assert(incr(List(1)) == List(2))
    assert(incr(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5, 6))
  }

  test("testIncr1") {
    assert(incr1(Nil) == Nil)
    assert(incr1(List(1)) == List(2))
    assert(incr1(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5, 6))
  }

  test("testDoubleToString") {
    assert(doubleToString(Nil) == Nil)
    assert(doubleToString(List(1.0)) == List("1.0"))
    assert(doubleToString(List(1.0, 2.0, 3.0, 4.0)) == List("1.0", "2.0", "3.0", "4.0"))
  }

  test("testMap") {
    assert(map(Nil: List[Int])(_ + 1) == Nil)
    assert(map(List(1))(_ + 1) == List(2))
    assert(map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
    assert(map(Nil: List[Double])(_.toString) == Nil)
    assert(map(List(1.0))(_.toString) == List("1.0"))
    assert(map(List(1.0, 2.0, 3.0))(_.toString) == List("1.0", "2.0", "3.0"))
  }

  test("testFilter") {
    assert(filter(Nil: List[Int])(_ % 2 == 0) == Nil)
    assert(filter(List(1))(_ % 2 == 0) == Nil)
    assert(filter(List(1, 2))(_ % 2 == 0) == List(2))
    assert(filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
  }

}
