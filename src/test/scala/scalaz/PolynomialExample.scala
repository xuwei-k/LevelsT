package scalaz

import scalaz.Id.Id
import scalaz.syntax.bind._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.LevelsT.T

object PolynomialExample {

  type LevelsUnit = LevelsT[Id, Unit]

  def main(args: Array[String]): Unit = {
    val x1 = toUnitValues(1, 0, 2) ++ toUnitValues(2, 0, 0, 1)
    println(x1)
    println(unitToList(x1))
    assert(unitToList(x1) === IList(3, 0, 2, 1))

    val x2 = toUnitValues(1, 0, 2) >> toUnitValues(2, 0, 1)
    println(x2)
    println(unitToList(x2))
    assert(unitToList(x2) === IList(2, 0, 5, 0, 2))
  }

  def unitToList(x: LevelsUnit): IList[Int] = {
    x.run match {
      case Maybe.Just(T(x, xs)) =>
        Foldable[Bag].length(x) :: unitToList(xs)
      case Maybe.Empty() =>
        IList.empty[Int]
    }
  }

  def toUnitValues(xs: Int*): LevelsUnit = {
    def treeN(n: Int): BTree[Unit] = {
      if (n == 1) {
        BTree.Leaf(())
      } else {
        BTree.Node(treeN(n - 1), BTree.Leaf(()))
      }
    }

    xs.map {
      case 0 =>
        Bag.empty[Unit]
      case n =>
        Bag.Value(treeN(n))
    }.foldRight(empty)((x, y) => LevelsT[Id, Unit](Maybe.just(T(x, y))))
  }

  val empty: LevelsUnit =
    LevelsT[Id, Unit](Maybe.empty[T[Id, Unit]])
}
