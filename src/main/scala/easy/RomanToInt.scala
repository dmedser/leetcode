package easy

object RomanToInt extends App {

  // format: off
  /**
   * Symbol       Value
   * I             1
   * V             5
   * X             10
   * L             50
   * C             100
   * D             500
   * M             1000
   * 
   * I can be placed before V (5) and X (10) to make 4 and 9. 
   * X can be placed before L (50) and C (100) to make 40 and 90. 
   * C can be placed before D (500) and M (1000) to make 400 and 900.
   *
   */
  // format: on    

  val dict =
    Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )

  val set =
    Set(
      ('I', 'V'),
      ('I', 'X'),
      ('X', 'L'),
      ('X', 'C'),
      ('C', 'D'),
      ('C', 'M')
    )

  def romanToInt(s: String): Int =
    s.foldLeft(
      (0, '0')
    ) { case ((res, pre), curr) =>
      lazy val preN = dict.getOrElse(pre, 0)
      val currN     = dict.getOrElse(curr, 0)

      val delta =
        if (set.contains((pre, curr)))
          currN - (2 * preN)
        else
          currN

      (res + delta, curr)
    }._1


  println(romanToInt("III"))
  println(romanToInt("IX"))
  println(romanToInt("XIX"))
  println(romanToInt("XXI"))
  println(romanToInt("LVIII"))
  println(romanToInt("MCMXCIV"))

}
