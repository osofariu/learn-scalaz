package day0.plus

case class Num(i: Int) extends Plus[Num] {

  def plus(a2: Num): Num = {
    Num(a2.i + i)
  }
}
