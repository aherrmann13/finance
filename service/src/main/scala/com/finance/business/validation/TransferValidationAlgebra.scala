package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.transfer.Transfer
import com.finance.business.validation.errors._

trait TransferValidationAlgebra[F[_]] {
  def idIsNone(transfer: Transfer): EitherT[F, IdMustBeNone, Unit]

  def exists(transfer: Transfer): EitherT[F, DoesNotExist, Unit]

  def toAccountIdExists(transfer: Transfer): EitherT[F, DoesNotExist, Unit]

  def fromAccountIdExists(transfer: Transfer): EitherT[F, DoesNotExist, Unit]
}
