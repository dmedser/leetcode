package easy

import scala.annotation.tailrec

object LongestCommonPrefix extends App {

  def longestCommonPrefixOfTwoStrings(s0: String, s1: String): String = {
    @tailrec
    def go(s0: List[Char], s1: List[Char], commonPrefix: String): String = {
      (s0, s1) match {
        case (h0 :: t0, h1 :: t1) if h0 == h1 => go(t0, t1, commonPrefix.appended(h0))
        case _                                => commonPrefix
      }
    }

    go(s0.toList, s1.toList, "")
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    strs.headOption match {
      case Some(head) =>
        strs.tail.foldLeft(head) { case (commonPrefix, str) =>
          longestCommonPrefixOfTwoStrings(commonPrefix, str)
        }
      case None => ""
    }
  }

  println(longestCommonPrefix(Array("flower", "flow", "flight")))
  println(longestCommonPrefix(Array("dog", "racecar", "car")))

}
