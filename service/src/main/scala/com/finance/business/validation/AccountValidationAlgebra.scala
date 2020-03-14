package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.account.Account
import com.finance.business.model.types.Id
import com.finance.business.validation.errors._

trait AccountValidationAlgebra[F[_]] {
  def idIsNone(account: Account): EitherT[F, IdMustBeNone, Unit]

  def exists(account: Account): EitherT[F, DoesNotExist, Unit]

  def nameIsValid(account: Account): EitherT[F, NameTooLong, Unit]

  def descriptionIsValid(account: Account): EitherT[F, DescriptionTooLong, Unit]

  def accountTypeIsValid(account: Account): EitherT[F, AccountTypeInvalid, Unit]

  def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit]

  def hasNoAssets(account: Account): EitherT[F, HasAssets, Unit]

  def hasNoPaybacks(account: Account): EitherT[F, HasPaybacks, Unit]
}
