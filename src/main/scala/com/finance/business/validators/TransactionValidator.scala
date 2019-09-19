package com.finance.business.validators

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Monad}
import com.finance.business.common.Constants.MaxDescriptionLength
import com.finance.business.errors._
import com.finance.business.model.account.AccountRepository
import com.finance.business.model.source.SourceRepository
import com.finance.business.model.transaction.{Amount, Transaction, TransactionRepository}

object TransactionValidator {
  def apply[F[_]: Applicative](
      transactionRepository: TransactionRepository[F],
      sourceRepository: SourceRepository[F],
      accountRepository: AccountRepository[F]
  ) = new TransactionValidator[F](transactionRepository, sourceRepository, accountRepository)
}

class TransactionValidator[F[_]: Applicative](
    transactionRepository: TransactionRepository[F],
    sourceRepository: SourceRepository[F],
    accountRepository: AccountRepository[F]
) {
  def exists(transaction: Transaction): EitherT[F, BusinessError, Unit] =
    EitherT {
      transaction.id match {
        case Some(id) =>
          transactionRepository.get(transaction.userId, id).map {
            case Some(_) => Right(())
            case _ => Left(TransactionDoesNotExistError)
          }
        case _ => Either.left[BusinessError, Unit](TransactionDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(transaction: Transaction): EitherT[F, BusinessError, Unit] =
    EitherT {
      transaction.id match {
        case Some(id) =>
          transactionRepository.get(transaction.userId, id).map {
            case Some(_) => Left(TransactionAlreadyExistsError)
            case _ => Right(())
          }
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(transaction: Transaction)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    for {
      _ <- validateDesc(transaction.description)
      result <- validateAmount(transaction.amount)
    } yield result

  def sourceExists(transaction: Transaction): EitherT[F, BusinessError, Unit] =
    EitherT {
      sourceRepository.get(transaction.userId, transaction.sourceId).map {
        case Some(_) => Right(())
        case _ => Left(SourceDoesNotExistError)
      }
    }

  def accountExists(transaction: Transaction): EitherT[F, BusinessError, Unit] =
    EitherT {
      accountRepository.get(transaction.userId, transaction.accountId).map {
        case Some(_) => Right(())
        case _ => Left(AccountDoesNotExistError)
      }
    }

  private def validateAmount(amount: Seq[Amount])(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    amount match {
      case Nil => EitherT.rightT[F, BusinessError](())
      case x :: y =>
        validateAmount(x).flatMap { _ =>
          validateAmount(y)
        }
    }

  private def validateAmount(amount: Amount): EitherT[F, BusinessError, Unit] =
    validateDesc(amount.description)

  private def validateDesc(desc: String): EitherT[F, BusinessError, Unit] =
    EitherT {
      desc match {
        case x if x == null => Either.left[BusinessError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > MaxDescriptionLength =>
          Either.left[BusinessError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }
}
