

case class Add[T](numbers: T*)(implicit x: scala.math.Numeric[T]) {
  def eval: T = numbers.reduceLeft(x.plus)
}
Add(1,2)
Add(1.0, 2.5).eval
Add(1,2,3).eval

