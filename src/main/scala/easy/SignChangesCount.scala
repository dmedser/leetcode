package easy

object SignChangesCount extends App {
  /* сть список из интов. Подсчитать количество перемен знака в последовательности (ноль не перемена)
   */

  def count(input: List[Int]): Int = {

    def fold(head: Int, tail: List[Int]): Int = {
      val res =
        tail.foldLeft((head, 0)) { case ((l, acc), r) =>
          if ((l * r) >= 0)
            (r, acc)
          else
            (r, acc + 1)
        }
      res._2
    }

    val inputWithoutZero = input.filterNot(_ == 0)

    inputWithoutZero match {
      case h :: t => fold(h, t)
      case _      => 0
    }
  }

  val in  = List(1, -2, 3, 3, 4, -2, -2, -3, 1)
  val in0 = List()
  val in1 = List(1)
  val in2 = List(1, 2)
  val in3 = List(1, 2, -3)
  val in4 = List(1, 2, -3, 0)
  val in5 = List(1, 2, 0, -3)

  val col = count(in5)
  println(col)

}
