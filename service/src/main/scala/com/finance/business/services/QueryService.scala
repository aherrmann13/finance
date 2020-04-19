package com.finance.business.services

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

trait QueryService[F[_], M <: Model] {
  def get(id: Id): F[Option[M]]

  def getMany(ids: Seq[Id]): F[Seq[M]]

  def getAll: F[Seq[M]]
}
