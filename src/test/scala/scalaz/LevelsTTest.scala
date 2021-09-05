package scalaz

import scalaprops.Gen
import scalaprops.Scalaprops
import scalaprops.scalazlaws
import scalaz.std.AllInstances._
import scalaz.Id.Id
import scalaz.idInstance
import scalaprops.ScalapropsScalaz._

object LevelsTTest extends Scalaprops {

  implicit def bTreeGen[A](implicit A: Gen[A]): Gen[BTree[A]] =
    Gen.lazyFrequency(
      4 -> scalaprops.Lazy(A.map(BTree.leaf)),
      1 -> scalaprops.Lazy {
        val a = bTreeGen[A]
        Apply[Gen].apply2(a, a)(BTree.node)
      }
    )

  implicit def bagGen[A](implicit A: Gen[A]): Gen[Bag[A]] =
    Gen.frequency(
      3 -> Applicative[Gen].point(Bag.empty[A]),
      4 -> A.map(Bag.single),
      1 -> Gen[BTree[A]].map(Bag.Value.apply)
    )

  implicit def levelsTGen[F[_], A](implicit
      F: => Gen1[F],
      A: Gen[A]
  ): Gen[LevelsT[F, A]] = {
    Gen.gen[LevelsT[F, A]] { (size, r) =>
      implicit lazy val f0: Gen1[F] = F
      val g = f0.gen1[Maybe[(Bag[A], LevelsT[F, A])]].map(LevelsT.apply[F, A])
      g.f(size, r)
    }
  }

  val monadPlusId = scalazlaws.monadPlus.all[LevelsT[Id, *]]

  val monadPlusMaybe = scalazlaws.monadPlus.all[LevelsT[Maybe, *]]

  val equalMaybe = scalazlaws.equal.all[LevelsT[Maybe, Int]]

  val btree = scalazlaws.foldable.all[BTree]

  val bagFoldable = scalazlaws.foldable.all[Bag]
  val bagMonoid = scalazlaws.monoid.all[Bag[Byte]]
}
