package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.account.implicits._
import com.finance.business.model.category.implicits._
import com.finance.business.model.payback.implicits._
import com.finance.business.model.source.implicits._
import com.finance.business.model.transaction.implicits._
import com.finance.business.model.transaction.{Amount, CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.operations.CategoryOps._
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

  override def accountIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.accountId, accountRepository.get)

  override def amountDescAreValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit] =
    transaction.amounts.toList.traverse(PropertyValidator.descriptionIsValid[F, Amount](_)).as(())

  override def categoryIdsExist(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.collect {
      case c: CategoryAmount => c
    }.toList.traverse { catAmt =>
      PropertyValidator.exists(catAmt.categoryId, categoryRepository.get)
    }.as(())

  override def paybackIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.collect {
      case p: PaybackAmount => p
    }.toList.traverse { paybackAmt =>
      PropertyValidator.exists(paybackAmt.paybackId, paybackRepository.get)
    }.as(())

  override def sourceIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.toList.traverse(a => PropertyValidator.exists(a.sourceId, sourceRepository.get)).as(())

  // TODO: clean this up
  override def reportingDateWithinBudgetTime(transaction: Transaction): EitherT[F, DateNotInEffectiveTime, Unit] =
    transaction.amounts.collect {
      case c: CategoryAmount => c
    }.toList.traverse { catAmt =>
      EitherT {
        categoryRepository.get(catAmt.categoryId) map { category =>
          Either.cond(
            category.budget.flatMap(_.effectiveTime).exists(_ contains catAmt.reportingDate),
            (),
            DateNotInEffectiveTime(catAmt.reportingDate, category.budget.flatMap(_.effectiveTime))
          )
        } getOrElse Right(())
      }
    }.as(())
}
