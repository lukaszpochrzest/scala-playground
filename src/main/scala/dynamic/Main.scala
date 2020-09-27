package dynamic

object Main extends App {

  class Result(val minAbsSum: Int, val signs: List[Int]) {
    override def toString: String = s"Result((${signs.mkString(")+(")})=$minAbsSum)"
  }

  def minAbsSum(a: Seq[Int]): Result = minAbsSumRec(0, a, Nil)

  private def minAbsSumRec(minAbsSum: Int, a: Seq[Int], signs: List[Int]): Result = a match {
    case Nil => new Result(minAbsSum, signs)
    case _ =>
      val withPlus = minAbsSumRec(minAbsSum + a.head, a.tail, a.head :: signs)
      val withMinus = minAbsSumRec(minAbsSum - a.head, a.tail, -a.head :: signs)
      if (Math.abs(withPlus.minAbsSum) < Math.abs(withMinus.minAbsSum)) withPlus else withMinus
  }

  println(minAbsSum(Array(1, -5, 2, 5, 7)))
}











