package day0.numeric

import org.scalatest.{Matchers, path}

class NumericTest extends path.FunSpec with Matchers {

  describe("I can use implicits to get access to Numeric operations") {

    it("given implicit parameters, I  can add them using Numeric plus on Int") {

      def add(a: Int, b: Int)(implicit n: Numeric[Int]): Int = {
        n.plus(a, b)
      }

      add(10, 3) should be(13)
    }

    it("given a type T I can also use Numeric ops to add two numbers") {

      def add[T](a: T, b: T)(implicit n: Numeric[T]): T = {
        n.plus(a, b)
      }

      add(10, 3) should be(13)
    }

    it("") {
      case class Add[T](numbers: T*)(implicit x: scala.math.Numeric[T]) {
        def eval: T = numbers.reduceLeft(x.plus)
      }

      Add(1, 2).eval should be(3)
      Add(1.0, 2.0).eval should be(3.0)
    }
  }
}
