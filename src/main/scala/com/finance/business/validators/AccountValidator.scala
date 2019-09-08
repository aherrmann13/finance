package com.finance.business.validators

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Monad}
import com.finance.business.errors._
import com.finance.business.model.account._

object AccountValidator {
  def apply[F[_]: Applicative](repository: AccountRepository[F]) =
    new AccountValidator[F](repository)
}

class AccountValidator[F[_]: Applicative](repository: AccountRepository[F]) {

  def exists(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          repository.get(account.userId, id).map {
            case Some(_) => Right(())
            case _ => Left(AccountDoesNotExistError)
          }
        case _ => Either.left[BusinessError, Unit](AccountDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          repository.get(account.userId, id).map {
            case Some(_) => Left(AccountAlreadyExistsError)
            case _ => Right(())
          }
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(account: Account)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    for {
      _ <- validateName(account)
      result <- validateDesc(account)
    } yield result

  private def validateName(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.name match {
        case x if x == null => Either.left[BusinessError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > Account.maxNameLength =>
          Either.left[BusinessError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  private def validateDesc(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.description match {
        case x if x == null => Either.left[BusinessError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > Account.maxNameLength =>
          Either.left[BusinessError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

}
