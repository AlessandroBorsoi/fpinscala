package fpinscala.state

import org.scalatest.FunSuite

class RNGTest extends FunSuite {

  import fpinscala.state.RNG._

  test("testNonNegativeInt") {
    val (int, _) = nonNegativeInt(SimpleRNG(42))
    assert(int > 0)
  }

  test("testDouble") {
    val (d, _) = double(SimpleRNG(42))
    assert(0 < d && d < 1)
  }

  test("testDoubleWithMap") {
    val dwm: Rand[Double] = doubleWithMap
    val (d, _) = dwm(SimpleRNG(42))
    assert(d > 0 && d < 1)
  }

  test("testSequence") {
    assert(sequence(List(unit(1), unit(2), unit(3)))(SimpleRNG(42))._1 == List(1, 2, 3))
  }

}
