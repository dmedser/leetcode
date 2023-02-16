package easy

object PreMax extends App {

  def findPreMax(input: List[Int]): Option[Int] = {

    def fold(x0: Int, x1: Int, xs: List[Int]): Option[Int] = {
      val (preMax, preMin) = {
        xs.foldLeft((x0 min x1, x0 max x1)) { case ((preMax, max), e) =>
          if e > max then (max, e)
          else if e > preMax then (e, max)
          else (preMax, max)
        }
      }
      if (preMax == preMin) None
      else Some(preMax)
    }

    input match
      case x0 :: x1 :: xs => fold(x0, x1, xs)
      case _              => None
  }

  println(findPreMax(List.empty[Int]))
  println(findPreMax(List(1)))
  println(findPreMax(List(1, 1)))
  println(findPreMax(List(1, 2)))
  println(findPreMax(List(1, 2, 4)))
  println(findPreMax(List(1, 2, 4, -3, 0, 10)))
  println(findPreMax(List(-1, -2, -4, 3, 0, -10)))
  println(findPreMax(List(1, 1, 1)))
  println(findPreMax(List(1, 1, 2)))

}
