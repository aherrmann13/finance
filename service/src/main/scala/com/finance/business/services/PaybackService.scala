package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.payback.Payback
import com.finance.business.model.types.Id
import com.finance.business.repository.PaybackRepository
import com.finance.business.validation.PaybackValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class PaybackService[F[_]: Monad](
    validator: PaybackValidationAlgebra[F],
    repository: PaybackRepository[F]
) extends CommandService[F, Payback] {
  override def create(model: Payback): EitherT[F, ValidationError, Payback] =
    for {
      _ <- validator idIsNone model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Payback): EitherT[F, ValidationError, Payback] =
    for {
      _ <- validator exists model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    for {
      _ <- validator hasNoTransactions id
      deleted <- EitherT.liftF(repository delete id)
    } yield deleted
}
