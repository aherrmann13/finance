package com.finance.business.validators

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Monad}
import com.finance.business.common.Constants._
import com.finance.business.errors._
import com.finance.business.model.account._
import com.finance.business.model.transaction.TransactionRepository

object AccountValidator {
  def apply[F[_]: Applicative](
      accountRepository: AccountRepository[F],
      transactionRepository: TransactionRepository[F]
  ) = new AccountValidator[F](accountRepository, transactionRepository)
}

class AccountValidator[F[_]: Applicative](
    accountRepository: AccountRepository[F],
    transactionRepository: TransactionRepository[F]
) {

  def exists(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          accountRepository.get(account.userId, id).map {
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
          accountRepository.get(account.userId, id).map {
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

  def hasTransactions(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.id match {
        case Some(id) =>
          transactionRepository.anyWithAccountId(account.userId, id).map {
            if (_) Left(ReferencedByTransactionError) else Right(())
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  private def validateName(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.name match {
        case x if x == null => Either.left[BusinessError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > MaxNameLength =>
          Either.left[BusinessError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  private def validateDesc(account: Account): EitherT[F, BusinessError, Unit] =
    EitherT {
      account.description match {
        case x if x == null => Either.left[BusinessError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > MaxNameLength =>
          Either.left[BusinessError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

}
