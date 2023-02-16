package easy

import scala.annotation.tailrec

object LengthOfLastWord extends App {

  def lengthOfLastWord(s: String): Int = {
    @tailrec
    def go1(i: Int): Int = {
      if (s(i) == ' ') {
        go1(i - 1)
      } else
        i
    }

    val i = go1(s.length - 1)

    @tailrec
    def go2(i: Int, res: Int): Int = {
      if (scala.util.Try(s(i) != ' ').getOrElse(false))
        go2(i - 1, res + 1)
      else res
    }

    go2(i, 0)
  }

  println(lengthOfLastWord("   fly me   to   the moon  "))
  println(lengthOfLastWord("luffy is still joyboy"))
  println(lengthOfLastWord("Hello World"))
  println(lengthOfLastWord("a"))

}
