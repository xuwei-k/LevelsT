package scalaz

import scalaz.Id.Id
import scalaprops.Gen
import scalaprops.ScalapropsScalaz._

abstract class Gen1[F[_]] { self =>

  def gen1[A: Gen]: Gen[F[A]]

  final def mapF[G[_]](f: F ~> G): Gen1[G] =
    new Gen1[G] {
      def gen1[A: Gen]: Gen[G[A]] =
        self.gen1[A].map(f.apply)
    }
}

object Gen1 {
  def apply[F[_]](implicit F: Gen1[F]): Gen1[F] = F

  implicit val id: Gen1[Id] =
    new Gen1[Id] {
      def gen1[A: Gen] =
        Gen[A]
    }

  implicit val maybe: Gen1[Maybe] =
    new Gen1[Maybe] {
      def gen1[A: Gen] =
        Gen[Maybe[A]]
    }

  implicit val iList: Gen1[IList] =
    new Gen1[IList] {
      def gen1[A: Gen] =
        Gen[IList[A]]
    }
}
