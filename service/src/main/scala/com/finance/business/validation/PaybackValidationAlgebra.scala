package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.payback.Payback
import com.finance.business.model.types.Id
import com.finance.business.validation.errors._

trait PaybackValidationAlgebra[F[_]] {
  def idIsNone(payback: Payback): EitherT[F, IdMustBeNone, Unit]

  def exists(payback: Payback): EitherT[F, DoesNotExist, Unit]

  def nameIsValid(payback: Payback): EitherT[F, NameTooLong, Unit]

  def descriptionIsValid(payback: Payback): EitherT[F, DescriptionTooLong, Unit]

  def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit]
}
