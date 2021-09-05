package scalaz

import scalaz.Id.Id

trait Eq1[F[_]] {
  def eq1[A: Equal]: Equal[F[A]]
}

object Eq1 {
  def apply[F[_]](implicit F: Eq1[F]): Eq1[F] = F

  implicit val idEq1: Eq1[Id] =
    new Eq1[Id] {
      def eq1[A: Equal] = Equal[A]
    }

  implicit val maybeEq1: Eq1[Maybe] =
    new Eq1[Maybe] {
      def eq1[A: Equal] = Equal[Maybe[A]]
    }

  implicit val iListEq1: Eq1[IList] =
    new Eq1[IList] {
      def eq1[A: Equal] = Equal[IList[A]]
    }
}
