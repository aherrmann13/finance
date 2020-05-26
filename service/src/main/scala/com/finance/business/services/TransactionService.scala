package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import com.finance.business.model.transaction.Transaction
import com.finance.business.model.types.Id
import com.finance.business.repository.TransactionRepository
import com.finance.business.repository.query.TransactionQuery
import com.finance.business.validation.TransactionValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class TransactionService[F[_] : Monad](
  validator: TransactionValidationAlgebra[F],
  repository: TransactionRepository[F]
) extends CommandService[F, Transaction]
  with QueryService[F, Transaction] {
  override def create(model: Transaction): EitherT[F, ValidationError, Transaction] =
    for {
      _ <- validator idIsNone model
      _ <- validator descriptionIsValid model
      _ <- validator accountIdExists model
      _ <- validator amountDescAreValid model
      _ <- validator categoryIdsExist model
      _ <- validator paybackIdsExists model
      _ <- validator sourceIdsExists model
      _ <- validator reportingDateWithinBudgetTime model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Transaction): EitherT[F, ValidationError, Transaction] =
    for {
      _ <- validator exists model
      _ <- validator descriptionIsValid model
      _ <- validator accountIdExists model
      _ <- validator amountDescAreValid model
      _ <- validator categoryIdsExist model
      _ <- validator paybackIdsExists model
      _ <- validator sourceIdsExists model
      _ <- validator reportingDateWithinBudgetTime model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    EitherT.liftF(repository delete id)

  override def get(id: Id): OptionT[F, Transaction] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Transaction]] = repository.getMany(ids)

  override def getAll: F[Seq[Transaction]] = repository.getAll

  def get(query: TransactionQuery): F[Seq[Transaction]] = repository.get(query)
}
