package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.common.{RelationValidator, Service}
import com.finance.business.errors.BusinessError
import com.finance.business.model.source.{Source, SourceRepository}
import com.finance.business.validators.SourceValidator

object SourceService {
  def apply[F[_]](
      repository: SourceRepository[F],
      validator: SourceValidator[F],
      relationValidator: RelationValidator[F]
  ) =
    new SourceService[F](repository, validator, relationValidator)
}

class SourceService[F[_]](
    repository: SourceRepository[F],
    validator: SourceValidator[F],
    relationValidator: RelationValidator[F]
) extends Service[F, Source] {
  def create(source: Source)(implicit M: Monad[F]): EitherT[F, BusinessError, Source] =
    for {
      _ <- validator.propertiesAreValid(source)
      _ <- validator.doesNotExist(source)
      _ <- relationValidator.userExists(source)
      saved <- EitherT.liftF(repository.create(source))
    } yield saved

  def update(source: Source)(implicit M: Monad[F]): EitherT[F, BusinessError, Source] =
    for {
      _ <- validator.propertiesAreValid(source)
      _ <- validator.exists(source)
      _ <- relationValidator.userExists(source)
      saved <- EitherT.liftF(repository.update(source))
    } yield saved

  def delete(source: Source)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    source.id match {
      case Some(id) =>
        for {
          _ <- validator.hasTransactions(source)
          result <- EitherT.liftF(repository.delete(source.userId, id))
        } yield result
      case None => EitherT.rightT[F, BusinessError](())
    }

  def get(userId: Int, id: Int): F[Option[Source]] = repository.get(userId, id)

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Source]] = repository.getMany(userId, id)

  def getAll(userId: Int): F[Seq[Source]] = repository.getAll(userId)
}
