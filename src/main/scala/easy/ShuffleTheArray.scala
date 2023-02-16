package easy

object ShuffleTheArray extends App {
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val ab = scala.collection.mutable.ArrayBuilder.make[Int]
    var i  = 0
    while (i < n) {
      ab.addAll(Array(nums(i), nums(i + n)))
      i += 1
    }
    ab.result()
  }

  println(shuffle(Array(2, 5, 1, 3, 4, 7), 3).mkString(", "))
  println(shuffle(Array(1, 2, 3, 4, 4, 3, 2, 1), 4).mkString(", "))
  println(shuffle(Array(1, 1, 2, 2), 2).mkString(", "))

}
