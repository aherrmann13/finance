package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.types.Id
import com.finance.business.repository.TransferRepository
import com.finance.business.repository.query.TransferQuery
import com.finance.business.validation.TransferValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class TransferService[F[_]: Monad](
  validator: TransferValidationAlgebra[F],
  repository: TransferRepository[F]
) extends CommandService[F, Transfer]
    with QueryService[F, Transfer] {
  override def create(model: Transfer): EitherT[F, ValidationError, Transfer] =
    for {
      _ <- validator idIsNone model
      _ <- validator fromAccountIdExists model
      _ <- validator toAccountIdExists model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Transfer): EitherT[F, ValidationError, Transfer] =
    for {
      _ <- validator exists model
      _ <- validator fromAccountIdExists model
      _ <- validator toAccountIdExists model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    EitherT.liftF(repository delete id)

  override def get(id: Id): OptionT[F, Transfer] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Transfer]] = repository.getMany(ids)

  override def getAll: F[Seq[Transfer]] = repository.getAll

  def get(query: TransferQuery): F[Seq[Transfer]] = repository.get(query)
}
