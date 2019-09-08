package com.finance.business.validators

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Monad}
import com.finance.business.errors._
import com.finance.business.model.source._

object SourceValidator {
  def apply[F[_]: Applicative](repository: SourceRepository[F]) =
    new SourceValidator[F](repository)
}

class SourceValidator[F[_]: Applicative](repository: SourceRepository[F]) {

  def exists(source: Source): EitherT[F, BusinessError, Unit] =
    EitherT {
      source.id match {
        case Some(id) =>
          repository.get(source.userId, id).map {
            case Some(_) => Right(())
            case _ => Left(SourceDoesNotExistError)
          }
        case _ => Either.left[BusinessError, Unit](SourceDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(source: Source): EitherT[F, BusinessError, Unit] =
    EitherT {
      source.id match {
        case Some(id) =>
          repository.get(source.userId, id).map {
            case Some(_) => Left(SourceAlreadyExistsError)
            case _ => Right(())
          }
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(source: Source)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    for {
      _ <- validateName(source)
      result <- validateDesc(source)
    } yield result

  private def validateName(source: Source): EitherT[F, BusinessError, Unit] =
    EitherT {
      source.name match {
        case x if x == null => Either.left[BusinessError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > Source.maxNameLength =>
          Either.left[BusinessError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  private def validateDesc(source: Source): EitherT[F, BusinessError, Unit] =
    EitherT {
      source.description match {
        case x if x == null => Either.left[BusinessError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > Source.maxNameLength =>
          Either.left[BusinessError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

}
