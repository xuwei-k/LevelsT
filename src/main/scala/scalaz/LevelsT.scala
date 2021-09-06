package scalaz

import LevelsT.T

final case class LevelsT[F[_], A](run: F[Maybe[T[F, A]]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): LevelsT[F, B] =
    LevelsT[F, B](
      F.map(run)(
        _.map(_.map(f))
      )
    )

  def ++(that: LevelsT[F, A])(implicit F: Apply[F]): LevelsT[F, A] = {
    LevelsT[F, A](
      F.apply2(this.run, that.run) {
        case (Maybe.Empty(), ys) =>
          ys
        case (xs, Maybe.Empty()) =>
          xs
        case (Maybe.Just(T(x, xs)), Maybe.Just(T(y, ys))) =>
          Maybe.just(T(Semigroup[Bag[A]].append(x, y), xs ++ ys))
      }
    )
  }

  def flatMap[B](k: A => LevelsT[F, B])(implicit F: Monad[F]): LevelsT[F, B] =
    LevelsT[F, B](
      F.bind(run) {
        case Maybe.Empty() =>
          F.point(Maybe.empty[T[F, B]])
        case Maybe.Just(T(x, xs)) =>
          Foldable[Bag]
            .foldRight(
              x,
              LevelsT(F.point(Maybe.just(T(Bag.empty[B], xs.flatMap(k)))))
            ) { case (a, b) =>
              k(a) ++ b
            }
            .run
      }
    )
}

object LevelsT extends LevelsTInstances {
  final case class T[F[_], A](head: Bag[A], tail: LevelsT[F, A]) {
    def map[B](f: A => B)(implicit F: Functor[F]): T[F, B] =
      T(head.map(f), tail.map(f))
  }

  object T {
    implicit def equal[F[_]: Foldable, A: Order](implicit
        F: => Eq1[F],
        A: Equal[A]
    ): Equal[T[F, A]] = { case (a1 @ T(x, xs), a2 @ T(y, ys)) =>
      val F3 = Foldable[T[F, *]]
      if (F3.empty(a1)) {
        F3.empty(a2)
      } else {
        implicit lazy val f1: Eq1[F] = F
        Equal[Bag[A]].equal(x, y) && Equal[LevelsT[F, A]].equal(xs, ys)
      }
    }

    implicit def foldable[F[_]: Foldable]: Foldable[T[F, *]] =
      new Foldable.FromFoldMap[T[F, *]] {
        override def foldMap[A, B](
            fa: T[F, A]
        )(f: A => B)(implicit F: Monoid[B]): B =
          F.append(
            Foldable[Bag].foldMap(fa.head)(f),
            Foldable[LevelsT[F, *]].foldMap(fa.tail)(f)
          )
      }
  }

  implicit def foldable[F[_]: Foldable]: Foldable[LevelsT[F, *]] =
    new Foldable.FromFoldMap[LevelsT[F, *]] {
      override def foldMap[A, B](
          fa: LevelsT[F, A]
      )(f: A => B)(implicit F: Monoid[B]): B =
        Foldable[F].foldMap(fa.run)(a =>
          Foldable[Maybe].foldMap(a)(b => Foldable[T[F, *]].foldMap(b)(f))
        )
    }

  implicit def equal[F[_]: Foldable, A](implicit
      A: Order[A],
      F: => Eq1[F]
  ): Equal[LevelsT[F, A]] = { (a1: LevelsT[F, A], a2: LevelsT[F, A]) =>
    {
      implicit lazy val f0: Eq1[F] = F
      f0.eq1[Maybe[T[F, A]]].equal(a1.run, a2.run)
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
            Maybe.just(T(Bag.single(a), PlusEmpty[LevelsT[F, *]].empty[A]))
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
    LevelsT(F.point(Maybe.empty[T[F, A]]))
}

private trait LevelsTPlus[F[_]] extends Plus[LevelsT[F, *]] {
  implicit def F: Apply[F]

  override def plus[A](a: LevelsT[F, A], b: => LevelsT[F, A]): LevelsT[F, A] =
    a ++ b
}
