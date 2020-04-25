package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.account.implicits._
import com.finance.business.model.category.implicits._
import com.finance.business.model.payback.implicits._
import com.finance.business.model.source.implicits._
import com.finance.business.model.transaction.{Amount, CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.transaction.implicits._
import com.finance.business.repository._
import com.finance.business.validation.errors._

class TransactionValidationInterpreter[F[_] : Monad](
  transactionRepository: TransactionRepository[F],
  sourceRepository: SourceRepository[F],
  accountRepository: AccountRepository[F],
  categoryRepository: CategoryRepository[F],
  paybackRepository: PaybackRepository[F]
) extends TransactionValidationAlgebra[F] {
  override def idIsNone(transaction: Transaction): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(transaction)

  override def exists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.id, transactionRepository.get)

  override def descriptionIsValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(transaction)

  override def sourceIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.sourceId, sourceRepository.get)

  override def accountIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.accountId, accountRepository.get)

  override def amountDescAreValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit] =
    transaction.amounts.toList.traverse(PropertyValidator.descriptionIsValid[F, Amount](_)).map(_ => ())

  override def categoryIdsExist(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.toList.flatMap {
      case CategoryAmount(categoryId, _, _, _) => Some(categoryId)
      case _ => None
    }.traverse { x =>
      PropertyValidator.exists(x, categoryRepository.get)
    }.map(_ => ())

  override def paybackIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.toList.flatMap {
      case PaybackAmount(paybackId, _, _, _) => Some(paybackId)
      case _ => None
    }.traverse { x =>
      PropertyValidator.exists(x, paybackRepository.get)
    }.map(_ => ())
}
