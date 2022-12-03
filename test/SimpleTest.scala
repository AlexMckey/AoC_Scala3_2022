import org.scalatest.funsuite._

class SimpleTest extends AnyFunSuite:
  test("Samples") {
    assert(List(1,2,3).length == 3)
    val s = "А роза упала на лапу Азора".replace(" ", "").toLowerCase
    assert(s.reverse == s)
  }