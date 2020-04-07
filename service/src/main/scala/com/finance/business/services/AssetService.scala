package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.asset.{Asset, Stock}
import com.finance.business.model.types.Id
import com.finance.business.repository.AssetRepository
import com.finance.business.validation.AssetValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class AssetService[F[_] : Monad](
  validator: AssetValidationAlgebra[F],
  repository: AssetRepository[F]
) extends CommandService[F, Asset] {
  override def create(model: Asset): EitherT[F, ValidationError, Asset] =
    for {
      _ <- validator idIsNone model
      _ <- model match {
        case stock @ Stock(_, _, _) => validator stockActionsAreValid stock
        case _ => EitherT.rightT(())
      }
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Asset): EitherT[F, ValidationError, Asset] =
    for {
      _ <- validator exists model
      _ <- model match {
        case stock@Stock(_, _, _) => validator stockActionsAreValid stock
        case _ => EitherT.rightT(())
      }
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    EitherT.liftF(repository.delete(id))
}
