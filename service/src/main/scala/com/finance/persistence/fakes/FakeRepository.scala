package com.finance.persistence.fakes

import cats.Monad
import cats.implicits._
import cats.data.OptionT
import com.finance.business.model.base.Model
import com.finance.business.model.types.Id
import com.finance.business.repository.Repository

class FakeRepository[F[_]: Monad, M <: Model] extends Repository[F, M] {
  override def create(model: M): F[M] = model.pure[F]

  override def update(model: M): F[M] = model.pure[F]

  override def delete(id: Id): F[Unit] = ().pure[F]

  override def get(id: Id): OptionT[F, M] = OptionT.none

  override def getMany(ids: Seq[Id]): F[Seq[M]] = Seq.empty[M].pure[F]

  override def getAll: F[Seq[M]] = Seq.empty[M].pure[F]
}
