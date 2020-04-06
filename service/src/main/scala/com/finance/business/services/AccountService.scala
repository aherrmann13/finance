package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.account.Account
import com.finance.business.model.types.Id
import com.finance.business.repository.AccountRepository
import com.finance.business.validation.AccountValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class AccountService[F[_]: Monad](
  validator: AccountValidationAlgebra[F],
  repository: AccountRepository[F]
) extends CommandService[F, Account] {
  override def create(model: Account): EitherT[F, ValidationError, Account] =
    for {
      _ <- validator idIsNone model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Account): EitherT[F, ValidationError, Account] =
    for {
      _ <- validator exists model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      _ <- validator accountTypeIsValid model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    for {
      _ <- validator hasNoTransactions id
      _ <- validator hasNoAssets id
      _ <- validator hasNoPaybacks id
      deleted <- EitherT.liftF(repository delete id)
    } yield deleted
}
