package com.finance.business.source

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.implicits._

class SourceValidator[F[_]: Applicative](repository: SourceRepository[F]) {

  def exists(source: Source): EitherT[F, SourceValidationError, Unit] =
    EitherT {
      source.id match {
        case Some(id) =>
          repository.get(source.userId, id).map {
            case Some(_) => Right(())
            case _ => Left(SourceDoesNotExistError)
          }
        case _ => Either.left[SourceValidationError, Unit](SourceDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(source: Source): EitherT[F, SourceValidationError, Unit] =
    EitherT {
      source.id match {
        case Some(id) =>
          repository.get(source.userId, id).map {
            case Some(_) => Left(SourceAlreadyExistsError)
            case _ => Right(())
          }
        case _ => Either.right[SourceValidationError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(source: Source)(implicit M: Monad[F]): EitherT[F, SourceValidationError, Unit] =
    for {
      _ <- validateName(source)
      result <- validateDesc(source)
    } yield result

  private def validateName(source: Source): EitherT[F, SourceValidationError, Unit] =
    EitherT {
      source.name match {
        case x if x == null => Either.left[SourceValidationError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > Source.maxNameLength =>
          Either.left[SourceValidationError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[SourceValidationError, Unit](()).pure[F]
      }
    }

  private def validateDesc(source: Source): EitherT[F, SourceValidationError, Unit] =
    EitherT {
      source.description match {
        case x if x == null => Either.left[SourceValidationError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > Source.maxNameLength =>
          Either.left[SourceValidationError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[SourceValidationError, Unit](()).pure[F]
      }
    }

}


object SourceValidator {
  def apply[F[_]: Applicative](repository: SourceRepository[F]) =
    new SourceValidator[F](repository)
}
