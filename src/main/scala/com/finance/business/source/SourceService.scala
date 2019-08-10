package com.finance.business.source

import cats.Monad
import cats.data.EitherT

class SourceService[F[_]](
    repository: SourceRepository[F],
    validator: SourceValidator[F]
) {
  def create(source: Source)(implicit M: Monad[F]): EitherT[F, SourceValidationError, Source] =
    for {
      _ <- validator.propertiesAreValid(source)
      _ <- validator.doesNotExist(source)
      saved <- EitherT.liftF(repository.create(source))
    } yield saved

  def update(source: Source)(implicit M: Monad[F]): EitherT[F, SourceValidationError, Source] =
    for {
      _ <- validator.propertiesAreValid(source)
      _ <- validator.exists(source)
      saved <- EitherT.liftF(repository.update(source))
    } yield saved

  def delete(userId: Int, id: Int): F[Unit] = repository.delete(userId, id)

  def get(userId: Int, id: Int): F[Option[Source]] = repository.get(userId, id)

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Source]] = repository.getMany(userId, id)

  def getAll(userId: Int): F[Seq[Source]] = repository.getAll(userId)
}

object SourceService {
  def apply[F[_]](repository: SourceRepository[F], validator: SourceValidator[F]) =
    new SourceService[F](repository, validator)
}
