package tree

sealed trait Tree[+A]:
  def size: Int = 0

case class Leaf[A](var value: A) extends Tree[A]:
  override def size = 1

case class Node[A](var value: A, left: Tree[A], right: Tree[A]) extends Tree[A]:
  override def size = left.size + right.size + 1


object Tree {
  case object Empty extends Tree[Nothing]

  def apply[A](values: A*): Tree[A] = {
    values match
      case Nil       => Empty
      case Seq(head) => Leaf(head)

      case head +: tail => {
        val list = head +: tail
        val size = list.length
        val mid = size / 2
        val (left, right) = list.splitAt(mid)
        Node(list(mid), apply(left*), apply(right.tail*))
      }
  }

  extension [A](tree: Tree[A]) {
    infix def +(n: A): Tree[A] = {
      tree match {
        case Node(value, left, Empty)  => Node(value, left, Leaf(n))
        case Node(value, Empty, right) => Node(value, Leaf(n), right)
        case Node(value, left, right) =>
          if left.size <= right.size then Node(value, left + n, right)
          else Node(value, left, right + n)
        case Empty       => Leaf(n)
        case Leaf(value) => Node(value, Leaf(n), Empty)
      }
    }

    def contains(v: Int): Boolean = {
      tree match {
        case Empty       => false
        case Leaf(value) => v == value
        case Node(value, left, right) =>
          value == v || left.contains(v) || right.contains(v)
      }
    }

    def foreach(f: A => Unit): Unit = {
      tree.match
        case Empty       => {}
        case Leaf(value) => f(value)
        case Node(value, left, right) => {
          f(value)
          left.foreach(f)
          right.foreach(f)
        }
    }

    def copy(): Tree[A] = {
      tree match {
        case Empty                    => tree
        case Leaf(value)              => Leaf(value)
        case Node(value, left, right) => Node(value, left.copy(), right.copy())
      }
    }
  }
}
