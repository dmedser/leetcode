package easy

import scala.annotation.tailrec

object PrintTree extends App {
  /*
    1.
    Есть бинарное дерево, заданное указателем на вершину.
    Нужно написать функцию, которая печатает его уровень за уровнем.
    Например, есть такое дерево:
        A
       / \
      B   C
     / \   \
    D   E   F
    На экран нужно вывести:
    A
    B C
    D E F
   */

  sealed trait BTree

  final case class Node(l: BTree, r: BTree, value: String) extends BTree

  case object Null extends BTree

  def printBTree(bt: BTree): Unit = {

    @tailrec
    def go(bts: List[BTree]): Unit = {
      if (bts.isEmpty) ()
      else {
        val tier =
          bts.foldLeft(List.empty[BTree]) {
            case (acc, Node(l, r, v)) =>
              print(v)
              acc ::: (l :: r :: Nil)
            case (acc, Null) =>
              acc
          }
        print("\n")
        go(tier)
      }
    }

    go(List(bt))
  }

  printBTree(Node(Node(Node(Null, Null, "D"), Node(Null, Null, "E"), "B"), Node(Null, Node(Null, Null, "F"), "C"), "A"))
}
