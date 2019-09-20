package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.common.{RelationValidator, Service}
import com.finance.business.errors.BusinessError
import com.finance.business.model.transaction.{Transaction, TransactionRepository}
import com.finance.business.validators.TransactionValidator

object TransactionService {
  def apply[F[_]](
    repository: TransactionRepository[F],
    validator: TransactionValidator[F],
    relationValidator: RelationValidator[F]
  ) =
    new TransactionService[F](repository, validator, relationValidator)
}

class TransactionService[F[_]](
  repository: TransactionRepository[F],
  validator: TransactionValidator[F],
  relationValidator: RelationValidator[F]
) extends Service[F, Transaction] {
  def create(transaction: Transaction)(implicit M: Monad[F]): EitherT[F, BusinessError, Transaction] =
    for {
      _ <- validator.propertiesAreValid(transaction)
      _ <- validator.doesNotExist(transaction)
      _ <- relationValidator.userExists(transaction)
      _ <- validator.accountExists(transaction)
      _ <- validator.sourceExists(transaction)
      saved <- EitherT.liftF(repository.create(transaction))
    } yield saved

  def update(transaction: Transaction)(implicit M: Monad[F]): EitherT[F, BusinessError, Transaction] =
    for {
      _ <- validator.propertiesAreValid(transaction)
      _ <- validator.exists(transaction)
      _ <- relationValidator.userExists(transaction)
      _ <- validator.accountExists(transaction)
      _ <- validator.sourceExists(transaction)
      saved <- EitherT.liftF(repository.update(transaction))
    } yield saved

  def delete(transaction: Transaction)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    transaction.id match {
      case Some(id) => EitherT.liftF(repository.delete(transaction.userId, id))
      case None => EitherT.rightT[F, BusinessError](())
    }

  override def get(userId: Int, id: Int): F[Option[Transaction]] = repository.get(userId, id)

  override def getMany(userId: Int, id: Seq[Int]): F[Seq[Transaction]] = repository.getMany(userId, id)

  override def getAll(userId: Int): F[Seq[Transaction]] = repository.getAll(userId)
}
