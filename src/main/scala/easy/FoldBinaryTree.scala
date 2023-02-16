package easy

object FoldBinaryTree extends App {

  sealed trait Tree
  case class Node(v: Int, left: Tree, right: Tree) extends Tree
  case object Nil                                  extends Tree

  object Tree {
    // TODO: имплементировать fold
    def fold[B](tree: Tree, zero: B)(f: (Int, B, B) => B): B = {

      def go(tree: Tree): B = {
        tree match {
          case Node(v, l, r) => {
            f(v, go(l), go(r))
          }
          case Nil => zero
        }
      }

      go(tree)
    }
  }

  import Tree._

  val in = Node(1, Node(3, Nil, Node(1, Nil, Nil)), Node(2, Nil, Nil))
  // TODO: подсчитать сумму в узлах
  val sum = fold(in, 0) { case (v, l, r) =>
    v + l + r
  }

  val prod = fold(in, 1) { case (v, l, r) =>
    v * l * r
  }

  println(in)
  println(sum)
  println(prod)

}
