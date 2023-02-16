package easy

import scala.annotation.tailrec

object PlusOne extends App {

  def plusOne(digits: Array[Int]): Array[Int] = {

    @tailrec
    def go(i: Int, delta: Int, acc: List[Int]): List[Int] = {
      val newValue = digits(i) + delta
      if (i == 0) {
        if (newValue == 10)
          1 :: 0 :: acc
        else
          newValue :: acc
      } else {
        if (newValue == 10)
          go(i - 1, 1, 0 :: acc)
        else
          go(i - 1, 0, newValue :: acc)
      }
    }

    go(digits.length - 1, 1, List.empty[Int]).toArray
  }

  println(plusOne(Array(9)).mkString(","))
  println(plusOne(Array(1, 0, 9)).mkString(","))
  println(plusOne(Array(1, 0, 9, 9)).mkString(","))
  println(plusOne(Array(1, 9, 9, 9)).mkString(","))
  println(plusOne(Array(9, 9, 9, 9)).mkString(","))
}
