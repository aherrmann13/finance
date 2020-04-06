package com.finance.business.services

import cats.data.EitherT
import com.finance.business.model.base.Model
import com.finance.business.model.types.Id
import com.finance.business.validation.errors.ValidationError

trait CommandService[F[_], M <: Model] {
  def create(model: M): EitherT[F, ValidationError, M]

  def update(model: M): EitherT[F, ValidationError, M]

  def delete(id: Id): EitherT[F, ValidationError, Unit]
}
