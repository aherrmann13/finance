package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import com.finance.business.model.source.Source
import com.finance.business.model.types.Id
import com.finance.business.repository.SourceRepository
import com.finance.business.validation.SourceValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class SourceService[F[_] : Monad](
  validator: SourceValidationAlgebra[F],
  repository: SourceRepository[F]
) extends CommandService[F, Source]
  with QueryService[F, Source] {
  override def create(model: Source): EitherT[F, ValidationError, Source] =
    for {
      _ <- validator idIsNone model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Source): EitherT[F, ValidationError, Source] =
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

  override def get(id: Id): OptionT[F, Source] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Source]] = repository.getMany(ids)

  override def getAll: F[Seq[Source]] = repository.getAll

  def get(fuzzy: String): F[Seq[Source]] = repository.getFuzzyMatch(fuzzy)
}
