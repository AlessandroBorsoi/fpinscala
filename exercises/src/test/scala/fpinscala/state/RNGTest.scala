package fpinscala.state

import org.scalatest.FunSuite

class RNGTest extends FunSuite {

  import fpinscala.state.RNG._

  test("testNonNegativeInt") {
    val (int, _) = nonNegativeInt(SimpleRNG(42))
    assert(int > 0)
  }

}
