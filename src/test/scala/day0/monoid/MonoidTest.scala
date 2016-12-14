package day0.monoid

import org.scalatest.{Matchers, path}

class MonoidTest extends path.FunSpec with Matchers {

  val list = List(1, 2, 3, 4)

  describe("addition and multiplication operations without Monoids") {
    it("adding with loop") {
      var sum = 0
      list.foreach(i => sum += i)
      sum should be(10)
    }
    it("multiplying with loop") {
      var mul = 1
      list.foreach(i => mul = mul * i)
      mul should be(24)
    }
  }
  describe("using monoid explicitly") {
    describe("we can separate the loop from the operations") {

      trait Monoid {
        def mzero: Int
        def mappend(a: Int, b: Int): Int
      }

      object IntAddMonoid extends Monoid {
        def mzero = 0
        def mappend(a: Int, b: Int): Int = a + b
      }

      object IntMulMonoid extends Monoid {
        def mzero = 1
        def mappend(a: Int, b: Int): Int = a * b
      }

      it("adds the same with monoid") {
        def plus(l: List[Int]) = {
          l.fold(IntAddMonoid.mzero)(IntAddMonoid.mappend)
        }
        plus(list) should be(10)
      }
      it("multiplies, producing the same result using monoids") {
        def mul(l: List[Int]) = {
          l.fold(IntMulMonoid.mzero)(IntMulMonoid.mappend)
        }
        mul(list) should be(24)
      }
    }
    describe("let's pass the monoid to the loop") {

      trait Monoid {
        def mzero: Int
        def mappend(a: Int, b: Int): Int
      }

      object IntAddMonoid extends Monoid {
        def mzero = 0
        def mappend(a: Int, b: Int): Int = a + b
      }

      object IntMulMonoid extends Monoid {
        def mzero = 1
        def mappend(a: Int, b: Int): Int = a * b
      }

      object IntOps {
        def op(l: List[Int])(m: Monoid): Int = {
          l.fold(m.mzero)(m.mappend)
        }
      }

      it("can now use the same operation generically passing it the appropriate monoid") {
        IntOps.op(list)(IntAddMonoid) should be(10)
        IntOps.op(list)(IntMulMonoid) should be(24)
      }
    }
  }

  describe("using monoid implicitly") {
    describe("monoid is bound to a specific type T") {

      trait SimpleMonoid[T] {
        def zero: T
        def append(a: T, b: T): T
      }

      trait SimpleAddMonoid[T] extends SimpleMonoid[T] {}
      trait SimpleMultiplyMonoid[T] extends SimpleMonoid[T] {}


      class AddIntMonoid extends SimpleAddMonoid[Int] {
        def zero = 0
        def append(a: Int, b: Int): Int = a + b
      }

      class AddDoubleMonoid extends SimpleAddMonoid[Double] {
        def zero = 0.0
        def append(a: Double, b: Double): Double = a + b
      }

      class MultiplyIntMonoid extends SimpleMultiplyMonoid[Int] {
        def zero = 1
        def append(a: Int, b: Int): Int = a * b
      }

      implicit val addIntMonoid = new AddIntMonoid
      implicit val multiplyIntMonoid = new MultiplyIntMonoid

      implicit val addDoubleMonoid = new AddDoubleMonoid

      class MathOps[T] {
        def plus(l: List[T])(implicit m: SimpleAddMonoid[T]): T = {
          l.fold(m.zero)(m.append)
        }
        def multiply(l: List[T])(implicit m: SimpleMultiplyMonoid[T]): T = {
          l.fold(m.zero)(m.append)
        }
      }

      def l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      it("MathOps will add a list of Integers") {
        new MathOps().plus(l) should be(55)
      }

      it("MathOps will multiply list of Integers") {
        new MathOps().multiply(l) should be(3628800)
      }

      def ld = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.3, 8.4, 9.3)
      it("MathOps will add a list of Doubles") {
        new MathOps().plus(ld) should be(46.0)
      }
    }
    describe("monoid is generic on type T") {

      trait NumericMonoid[T] {
        def zero(implicit n: Numeric[T]) : T
        def append(a: T, b: T)(implicit n: Numeric[T]) : T
      }

      object NumericMonoid {
        implicit def addMonoid[T] : NumericMonoid[T] = new NumericMonoid[T] {
          override def append(a: T, b: T)(implicit n: Numeric[T]) = n.plus(a, b)
          override def zero(implicit n: Numeric[T]) = n.zero
        }
      }

      // TODO: can I generalize this further to work with any numeric type?
      object MathOps {
        def plus(l: List[Int])(implicit m: NumericMonoid[Int]): Int = {
          l.fold(m.zero)(m.append)
        }
      }

      it("MathOps will add a list of Integers") {
        MathOps.plus(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) should be(55)
      }
    }
  }
}
