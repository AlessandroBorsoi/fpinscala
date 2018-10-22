package fpinscala.datastructures

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  import fpinscala.datastructures.Tree._

  test("testSize") {
    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
  }

  test("testMaximum") {
    assert(maximum(Leaf(1)) == 1)
    assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
  }

  test("testDepth") {
    assert(depth(Leaf(1)) == 0)
    assert(depth(Branch(Leaf(1), Leaf(2))) == 1)
    assert(depth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(2))) == 2)
  }

  test("testMap") {
    assert(map(Leaf(1))(_ + 1) == Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(a => a * a) == Branch(Leaf(1), Leaf(4)))
  }

  test("testSize1") {
    assert(size1(Leaf(1)) == 1)
    assert(size1(Branch(Leaf(1), Leaf(2))) == 3)
  }

  test("testMaximum1") {
    assert(maximum1(Leaf(1)) == 1)
    assert(maximum1(Branch(Leaf(1), Leaf(2))) == 2)
  }

  test("testDepth1") {
    assert(depth1(Leaf(1)) == 0)
    assert(depth1(Branch(Leaf(1), Leaf(2))) == 1)
    assert(depth1(Branch(Branch(Leaf(3), Leaf(4)), Leaf(2))) == 2)
  }

  test("testMap1") {
    assert(map1(Leaf(1))(_ + 1) == Leaf(2))
    assert(map1(Branch(Leaf(1), Leaf(2)))(a => a * a) == Branch(Leaf(1), Leaf(4)))
  }

}
