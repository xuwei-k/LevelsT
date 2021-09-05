package scalaz

import scalaprops.Gen
import scalaprops.Scalaprops
import scalaprops.scalazlaws
import scalaz.std.AllInstances._
import scalaprops.ScalapropsScalaz._

object LevelsTTest extends Scalaprops {

  implicit def levelsTGen[F[_], A](implicit
      F: => Gen1[F],
      A: Gen[A]
  ): Gen[LevelsT[F, A]] = {
    Gen.gen[LevelsT[F, A]] { (size, r) =>
      implicit lazy val f0: Gen1[F] = F
      val g = f0.gen1[Maybe[(IList[A], LevelsT[F, A])]].map(LevelsT.apply[F, A])
      g.f(size, r)
    }
  }

  val monadPlusMaybe = scalazlaws.monadPlus.all[LevelsT[Maybe, *]]

  val equalMaybe = scalazlaws.equal.all[LevelsT[Maybe, Int]]

}
