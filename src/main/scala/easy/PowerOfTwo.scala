package easy

object PowerOfTwo extends App {
  def isPowerOfTwo(n: Int): Boolean = {
    if (n == 0) false
    else {
      var shifted = n
      while (shifted.&(1) != 1) {
        shifted >>= 1
      }
      shifted == 1
    }
  }

  println(isPowerOfTwo(8))
  println(isPowerOfTwo(7))
  println(isPowerOfTwo(1))
  println(isPowerOfTwo(35))
  println(isPowerOfTwo(32))
}
