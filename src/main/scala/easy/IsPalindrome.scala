package easy

import scala.annotation.tailrec

object IsPalindrome extends App {
  def isPalindromeInt(x: Int): Boolean = {
    var res = 0
    var i   = x
    while (i > 0) {
      res = res * 10 + i % 10
      i /= 10
    }
    res == x
  }

  def isPalindromeStr(x: String): Boolean = {
    @tailrec
    def go(chars: List[Char], acc: List[Char]): List[Char] =
      chars match {
        case Nil => acc
        case c :: cs => go(cs, c :: acc)
      }

    go(x.toList, List.empty[Char]).mkString == x
  }

  println(isPalindromeInt(721))
  println(isPalindromeInt(121))
  println(isPalindromeInt(-121))
  println(isPalindromeInt(10))
  
  println(isPalindromeStr("abc"))
  println(isPalindromeStr("abccba"))
  println(isPalindromeStr("abc1cba"))
  println(isPalindromeStr("a"))
}
