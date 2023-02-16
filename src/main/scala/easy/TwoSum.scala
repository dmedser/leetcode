package easy

object TwoSum extends App {
  
  // format: off
  /**
   * Input: nums = [2,7,11,15], target = 9
   * Output: [0,1]
   * Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
   */
  // format: on

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    def traverse(i: Int): Array[Int] = {
      val delta = target - nums(i)
      val j     = nums.indexOf(delta, from = i + 1)
      if (j > 0)
        Array(i, j)
      else
        Array.empty[Int]
    }

    var i = 0
    while (i < (nums.length - 1)) {
      val res = traverse(i)
      if (res.nonEmpty) return res
      i += 1
    }

    Array.empty[Int]
  }

  println(twoSum(Array(2, 7, 11, 15), 9).mkString(", "))
  println(twoSum(Array(3, 2, 4), 6).mkString(", "))
  println(twoSum(Array(3, 3), 6).mkString(", "))
  println(twoSum(Array(1, 2, 4, 5, 7, 9, 12), 9).mkString(", "))
}
