package easy

import scala.annotation.tailrec

object MySqrt extends App {
  def mySqrt(x: Int): Int = {
    @tailrec
    def go(l: Int, r: Int): Int = {
      if (l > r) r
      else {
        val mid = (l + r) / 2
        val div = x / mid
        if (div == mid) {
          mid
        } else if (div < mid)
          go(l, mid - 1)
        else
          go(mid + 1, r)
      }
    }

    go(1, x)
  }

  println(mySqrt(3))
  println(mySqrt(8))
  println(mySqrt(9))
  println(mySqrt(16))
  println(mySqrt(17))
}
