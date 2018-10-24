package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  import fpinscala.errorhandling.Option._

  test("testVariance") {
    assert(variance(List(1, 2, 3, 4, 5)) == Some(2))
    assert(variance(List(34, 65, 135, 6, 98, 222, 12)) == Some(5133.34693877551))
  }

  test("testSequence") {
    assert(sequence(List(None)) == None)
    assert(sequence(List(Some(1), Some(2), None)) == None)
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

}
