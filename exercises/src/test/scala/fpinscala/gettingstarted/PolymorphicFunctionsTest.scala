package fpinscala.gettingstarted

import org.scalatest.FunSuite

class PolymorphicFunctionsTest extends FunSuite {

  test("test is sorted empty array Int") {
    assert(PolymorphicFunctions.isSorted[Int](Array(), _ < _))
  }

  test("test is sorted array of one Int") {
    assert(PolymorphicFunctions.isSorted[Int](Array(1), _ < _))
  }

  test("test is sorted array of Int") {
    assert(PolymorphicFunctions.isSorted[Int](Array(1, 2, 3, 4), _ < _))
  }

  test("test is not sorted array of Int") {
    assert(!PolymorphicFunctions.isSorted[Int](Array(1, 2, 5, 4), _ < _))
  }

  test("test is sorted array of String") {
    assert(PolymorphicFunctions.isSorted[String](Array("a", "b", "c", "d"), _ < _))
  }

  test("test is not sorted array of String") {
    assert(!PolymorphicFunctions.isSorted[String](Array("one", "two", "three", "four"), _ < _))
  }

}
