package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.source.Source
import com.finance.business.model.types.Id
import com.finance.business.validation.errors._

trait SourceValidationAlgebra[F[_]] {
  def idIsNone(source: Source): EitherT[F, IdMustBeNone, Unit]

  def exists(source: Source): EitherT[F, DoesNotExist, Unit]

  def nameIsValid(source: Source): EitherT[F, NameTooLong, Unit]

  def descriptionIsValid(source: Source): EitherT[F, DescriptionTooLong, Unit]

  def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit]
}
