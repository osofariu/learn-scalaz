package day0.foldLeft

import org.scalatest.{Matchers, path}

class FoldLeftTest extends path.FunSpec with Matchers {

  describe("using foldLeft API") {

    trait Monoid[T] {
      def zero: T
      def append(x: T, y: T): T
    }

    object Monoid {
      implicit val IntMonoid : Monoid[Int] = new Monoid[Int] {
        override def zero: Int = 0
        override def append(x: Int, y: Int): Int = x + y
      }
    }

    trait FoldLeft[F[_]] {
      def fold[A, B](xs: F[A], a: B, f: (B, A) => B) : B
    }

    object FoldLeft {
      implicit val foldLeftList : FoldLeft[List]  = new FoldLeft[List] {
        override def fold[A, B](xs: List[A], a: B, f: (B, A) => B): B = xs.foldLeft(a)(f)
      }
    }

    it("FoldLeft called explicitly can add integers") {
      FoldLeft.foldLeftList.fold(List(1, 6), 0, (a: Int, b: Int) => a + b)
    }

    def sum[M[_] : FoldLeft, A: Monoid](xs: M[A]) : A = {
      val m = implicitly[Monoid[A]]
      val foldOp = implicitly[FoldLeft[M]]
      foldOp.fold(xs, m.zero, m.append)
    }

    it("sum can use a type class to add-up numbers by wrapping FoldLeft") {
      sum(List(1, 2, 3)) should be(6)
    }
  }
}
