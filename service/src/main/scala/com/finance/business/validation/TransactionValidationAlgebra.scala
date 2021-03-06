package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.transaction.Transaction
import com.finance.business.validation.errors._

trait TransactionValidationAlgebra[F[_]] {
  def idIsNone(transaction: Transaction): EitherT[F, IdMustBeNone, Unit]

  def exists(transaction: Transaction): EitherT[F, DoesNotExist, Unit]

  def descriptionIsValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit]

  def accountIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit]

  def amountDescAreValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit]

  def categoryIdsExist(transaction: Transaction): EitherT[F, DoesNotExist, Unit]

  def paybackIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit]

  def sourceIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit]

  def reportingDateWithinBudgetTime(transaction: Transaction): EitherT[F, DateNotInEffectiveTime, Unit]
}
