package com.finance.business.account

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.implicits._

class AccountValidator[F[_]: Applicative](repository: AccountRepository[F]) {

  def exists(account: Account): EitherT[F, AccountValidationError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          repository.get(account.userId, id).map {
            case Some(_) => Right(())
            case _ => Left(AccountDoesNotExistError)
          }
        //TODO: why does Left(AccountDoesNotExistError) not work?
        case _ => Either.left[AccountValidationError, Unit](AccountDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(account: Account): EitherT[F, AccountValidationError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          repository.get(account.userId, id).map {
            case Some(_) => Left(AccountAlreadyExistsError)
            case _ => Right(())
          }
        case _ => Either.right[AccountValidationError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(account: Account)(implicit M: Monad[F]): EitherT[F, AccountValidationError, Unit] =
    for {
      _ <- validateName(account)
      result <- validateDesc(account)
    } yield result

  private def validateName(account: Account): EitherT[F, AccountValidationError, Unit] =
    EitherT {
      account.name match {
        case x if x == null => Either.left[AccountValidationError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > Account.maxNameLength =>
          Either.left[AccountValidationError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[AccountValidationError, Unit](()).pure[F]
      }
    }

  private def validateDesc(account: Account): EitherT[F, AccountValidationError, Unit] =
    EitherT {
      account.description match {
        case x if x == null => Either.left[AccountValidationError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > Account.maxNameLength =>
          Either.left[AccountValidationError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[AccountValidationError, Unit](()).pure[F]
      }
    }

}

object AccountValidator {
  def apply[F[_]: Applicative](repository: AccountRepository[F]) =
    new AccountValidator[F](repository)
}
