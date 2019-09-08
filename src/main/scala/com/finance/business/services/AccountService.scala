package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.account.{Account, AccountRepository}
import com.finance.business.common.RelationValidator
import com.finance.business.errors.BusinessError
import com.finance.business.validators.AccountValidator

class AccountService[F[_]](
  repository: AccountRepository[F],
  validator: AccountValidator[F],
  relationValidator: RelationValidator[F]
) {
  def create(account: Account)(implicit M: Monad[F]): EitherT[F, BusinessError, Account] =
    for {
      _ <- validator.propertiesAreValid(account)
      _ <- validator.doesNotExist(account)
      _ <- relationValidator.userExists(account)
      saved <- EitherT.liftF(repository.create(account))
    } yield saved

  def update(account: Account)(implicit M: Monad[F]): EitherT[F, BusinessError, Account] =
    for {
      _ <- validator.propertiesAreValid(account)
      _ <- validator.exists(account)
      _ <- relationValidator.userExists(account)
      saved <- EitherT.liftF(repository.update(account))
    } yield saved

  def delete(userId: Int, id: Int): F[Unit] = repository.delete(userId, id)

  def get(userId: Int, id: Int): F[Option[Account]] = repository.get(userId, id)

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Account]] = repository.getMany(userId, id)

  def getAll(userId: Int): F[Seq[Account]] = repository.getAll(userId)
}

object AccountService {
  def apply[F[_]](
    repository: AccountRepository[F],
    validator: AccountValidator[F],
    relationValidator: RelationValidator[F]
  ) =
    new AccountService[F](repository, validator, relationValidator)
}
