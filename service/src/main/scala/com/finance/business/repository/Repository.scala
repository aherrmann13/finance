package com.finance.business.repository

import cats.data.OptionT
import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

trait Repository[F[_], M <: Model] {
  def create(model: M): F[M]

  def update(model: M): F[M]

  def delete(id: Id): F[Unit]

  def get(id: Id): OptionT[F, M]

  def getMany(ids: Seq[Id]): F[Seq[M]]

  def getAll: F[Seq[M]]
}
