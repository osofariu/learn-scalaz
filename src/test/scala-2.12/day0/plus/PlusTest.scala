package day0.plus

import day0.plus.PlusOps.plus
import org.scalatest.{Matchers, path}

class PlusTest extends path.FunSpec with Matchers {

  describe("Plus") {

    it("adds integers with ease") {
      val n1 = Num(12)
      val n2 = Num(13)
      plus(n1, n2) should be(Num(25))
    }
  }
}
