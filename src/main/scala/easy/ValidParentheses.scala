package easy

object ValidParentheses extends App {
  def isValid(s: String): Boolean = {

    if (s.length % 2 != 0)
      false
    else {

      val stack = scala.collection.mutable.Stack.empty[Char]

      def go(s: String): Boolean = {
        s.headOption match {
          case Some(c) if Set('(', '[', '{').contains(c) =>
            stack.push(c)
            go(s.tail)
          case Some(c) if c == ')' =>
            if (stack.headOption.contains('('))
              stack.pop()
              go(s.tail)
            else false
          case Some(c) if c == ']' =>
            if (stack.headOption.contains('['))
              stack.pop()
              go(s.tail)
            else false
          case Some(c) if c == '}' =>
            if (stack.headOption.contains('{'))
              stack.pop()
              go(s.tail)
            else false
          case _ => stack.isEmpty
        }
      }

      go(s)
    }
  }

  println(isValid("()"))
  println(isValid("()[]{}"))
  println(isValid("(]"))
}
