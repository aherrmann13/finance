package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.account.implicits._
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.transfer.implicits._
import com.finance.business.repository.{AccountRepository, TransferRepository}
import com.finance.business.validation.errors.{DoesNotExist, IdMustBeNone}

class TransferValidationInterpreter[F[_]: Monad](
    transferRepository: TransferRepository[F],
    accountRepository: AccountRepository[F]
) extends TransferValidationAlgebra[F] {
  override def idIsNone(transfer: Transfer): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(transfer)

  override def exists(transfer: Transfer): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transfer.id, transferRepository.get)

  override def toAccountIdExists(transfer: Transfer): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transfer.to, accountRepository.get)

  override def fromAccountIdExists(transfer: Transfer): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transfer.from, accountRepository.get)
}
