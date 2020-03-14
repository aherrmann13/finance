package com.finance.business.repository

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

trait Repository[F[_], M <: Model] {
  def create(model: M): F[M]

  def update(model: M): F[M]

  def delete(id: Id): F[Unit]

  def get(id: Id): F[Option[M]]

  def getMany(id: Seq[Int]): F[Seq[M]]

  def getAll: F[Seq[M]]
}
