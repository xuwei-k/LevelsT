package scalaz

sealed abstract class BTree[A] extends Product with Serializable {
  final def map[B](f: A => B): BTree[B] = this match {
    case BTree.Leaf(value) =>
      BTree.Leaf(f(value))
    case BTree.Node(a1, a2) =>
      BTree.Node(a1.map(f), a2.map(f))
  }
}

object BTree {
  final case class Leaf[A](value: A) extends BTree[A]
  final case class Node[A](a1: BTree[A], a2: BTree[A]) extends BTree[A]

  def leaf[A](a: A): BTree[A] = Leaf(a)
  def node[A](a1: BTree[A], a2: BTree[A]): BTree[A] = Node(a1, a2)

  implicit def equal[A](implicit A: Equal[A]): Equal[BTree[A]] =
    new Equal[BTree[A]] {
      override def equal(a1: BTree[A], a2: BTree[A]): Boolean = (a1, a2) match {
        case (Leaf(x1), Leaf(x2)) =>
          A.equal(x1, x2)
        case (Node(x1, x2), Node(y1, y2)) =>
          equal(x1, y1) && equal(x2, y2)
        case _ =>
          false
      }
    }

  implicit val foldable: Foldable[BTree] = new Foldable.FromFoldMap[BTree] {
    override def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit
        F: Monoid[B]
    ): B = {
      fa match {
        case Leaf(value) =>
          f(value)
        case Node(a1, a2) =>
          F.append(
            foldMap(a1)(f),
            foldMap(a2)(f)
          )
      }
    }
  }
}
