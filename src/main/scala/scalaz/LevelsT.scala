package scalaz

import scalaz.std.tuple._

final case class LevelsT[F[_], A](run: F[Maybe[(Bag[A], LevelsT[F, A])]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): LevelsT[F, B] =
    LevelsT[F, B](
      F.map(run)(
        _.map(Bifunctor[Tuple2].bimap(_)(_.map(f), _.map(f)))
      )
    )

  def ++(that: LevelsT[F, A])(implicit F: Apply[F]): LevelsT[F, A] = {
    LevelsT[F, A](
      F.apply2(this.run, that.run) {
        case (Maybe.Empty(), ys) =>
          ys
        case (xs, Maybe.Empty()) =>
          xs
        case (Maybe.Just((x, xs)), Maybe.Just((y, ys))) =>
          Maybe.just((Semigroup[Bag[A]].append(x, y), xs ++ ys))
      }
    )
  }

  def flatMap[B](k: A => LevelsT[F, B])(implicit F: Monad[F]): LevelsT[F, B] =
    LevelsT[F, B](
      F.bind(run) {
        case Maybe.Empty() =>
          F.point(Maybe.empty[(Bag[B], LevelsT[F, B])])
        case Maybe.Just((x, xs)) =>
          Foldable[Bag]
            .foldRight(
              x,
              LevelsT(F.point(Maybe.just((Bag.empty[B], xs.flatMap(k)))))
            ) { case (a, b) =>
              k(a) ++ b
            }
            .run
      }
    )
}

object LevelsT extends LevelsTInstances {
  implicit def equal[F[_], A](implicit
      A: Order[A],
      F: => Eq1[F]
  ): Equal[LevelsT[F, A]] = { (a1: LevelsT[F, A], a2: LevelsT[F, A]) =>
    {
      implicit lazy val f0: Eq1[F] = F
      f0.eq1[Maybe[(Bag[A], LevelsT[F, A])]].equal(a1.run, a2.run)
    }
  }

  implicit def monadPlus[F[_]](implicit
      F0: Monad[F]
  ): MonadPlus[LevelsT[F, *]] =
    new MonadPlus[LevelsT[F, *]] with LevelsTPlusEmpty[F] {
      override def F = F0

      override def point[A](a: => A): LevelsT[F, A] =
        LevelsT(
          F.point(
            Maybe.just((Bag.single(a), PlusEmpty[LevelsT[F, *]].empty[A]))
          )
        )

      override def map[A, B](fa: LevelsT[F, A])(f: A => B): LevelsT[F, B] =
        fa map f

      override def bind[A, B](fa: LevelsT[F, A])(
          f: A => LevelsT[F, B]
      ): LevelsT[F, B] =
        fa flatMap f
    }
}

sealed abstract class LevelsTInstances extends LevelsTInstances1 {
  implicit def plusEmpty[F[_]](implicit
      F0: Applicative[F]
  ): PlusEmpty[LevelsT[F, *]] =
    new LevelsTPlusEmpty[F] {
      override def F = F0
    }

}

sealed abstract class LevelsTInstances1 {
  implicit def plus[F[_]](implicit F0: Apply[F]): Plus[LevelsT[F, *]] =
    new LevelsTPlus[F] {
      override def F = F0
    }
}

private trait LevelsTPlusEmpty[F[_]]
    extends PlusEmpty[LevelsT[F, *]]
    with LevelsTPlus[F] {
  override def F: Applicative[F]

  override def empty[A]: LevelsT[F, A] =
    LevelsT(F.point(Maybe.empty[(Bag[A], LevelsT[F, A])]))
}

private trait LevelsTPlus[F[_]] extends Plus[LevelsT[F, *]] {
  implicit def F: Apply[F]

  override def plus[A](a: LevelsT[F, A], b: => LevelsT[F, A]): LevelsT[F, A] =
    a ++ b
}
