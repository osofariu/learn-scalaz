package day0.plus

object PlusOps {
  def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)
}
