package scalaz

sealed abstract class Bag[A] extends Product with Serializable {
  def map[B](f: A => B): Bag[B] = this match {
    case Bag.Empty() =>
      Bag.Empty()
    case Bag.Value(value) =>
      Bag.Value(value.map(f))
  }

  def toSortedList(implicit A: Order[A]): IList[A] = {
    Foldable[Bag].toIList(this).sorted
  }

}

object Bag {
  def empty[A]: Bag[A] = Empty()
  def single[A](a: A): Bag[A] = Value(BTree.Leaf(a))

  final case class Empty[A]() extends Bag[A]
  final case class Value[A](value: BTree[A]) extends Bag[A]

  implicit def equal[A: Order]: Equal[Bag[A]] =
    Equal.equalBy(_.toSortedList)

  implicit val foldable: Foldable[Bag] = new Foldable.FromFoldMap[Bag] {
    override def foldMap[A, B](
        fa: Bag[A]
    )(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Empty() =>
          F.zero
        case Value(value) =>
          Foldable[BTree].foldMap(value)(f)
      }
  }

  implicit def monoid[A]: Monoid[Bag[A]] =
    new Monoid[Bag[A]] {
      override def zero: Bag[A] = Empty()
      override def append(f1: Bag[A], f2: => Bag[A]): Bag[A] =
        (f1, f2) match {
          case (Empty(), ys) =>
            ys
          case (Value(_), Empty()) =>
            f1
          case (Value(xs), Value(ys)) =>
            Value(BTree.Node(xs, ys))
        }
    }

}
