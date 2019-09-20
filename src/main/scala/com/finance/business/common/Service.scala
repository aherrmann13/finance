package com.finance.business.common

import cats.Monad
import cats.data.EitherT
import com.finance.business.errors.BusinessError

trait Service[F[_], Item] {
  def create(item: Item)(implicit M: Monad[F]): EitherT[F, BusinessError, Item]
  def update(item: Item)(implicit M: Monad[F]): EitherT[F, BusinessError, Item]
  def delete(item: Item)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit]
  def get(userId: Int, id: Int): F[Option[Item]]
  def getMany(userId: Int, id: Seq[Int]): F[Seq[Item]]
  def getAll(userId: Int): F[Seq[Item]]
}
