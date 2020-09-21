package dynamic

object Main extends App {
  def absMin(in: Array[Int]): (Int, List[Boolean]) = {
    if (in.isEmpty) (0, Nil) else {
      val inList = in.toList
      val plus = minRec(inList.head, inList.tail, true :: Nil)
      val minus = minRec(-inList.head, inList.tail, false :: Nil)
      if (Math.abs(plus._1) < Math.abs(minus._1)) (plus._1, plus._2.reverse) else (minus._1, minus._2.reverse)
    }
  }

  def minRec(accumulator: Int, list: List[Int], binars: List[Boolean]): (Int, List[Boolean]) = {
    list match {
      case Nil => (accumulator, binars)
      case _ =>
        val plus = minRec(accumulator + list.head, list.tail, true :: binars)
        val minus = minRec(accumulator - list.head, list.tail, false :: binars)
        if (Math.abs(plus._1) < Math.abs(minus._1)) plus else minus
    }
  }

  println(absMin(Array(1, 5, 2, 2, 11)))
}











