package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.account.implicits._
import com.finance.business.model.category.implicits._
import com.finance.business.model.source.implicits._
import com.finance.business.model.transaction.{Amount, Transaction}
import com.finance.business.model.transaction.implicits._
import com.finance.business.repository.{AccountRepository, CategoryRepository, SourceRepository, TransactionRepository}
import com.finance.business.validation.errors.{DescriptionTooLong, DoesNotExist, IdMustBeNone, ValidationError}

class TransactionValidationInterpreter[F[_] : Monad](
  transactionRepository: TransactionRepository[F],
  sourceRepository: SourceRepository[F],
  accountRepository: AccountRepository[F],
  categoryRepository: CategoryRepository[F]
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
    validateAmounts[DescriptionTooLong](transaction.amounts, PropertyValidator.descriptionIsValid)

  override def categoryIdsExist(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    validateAmounts[DoesNotExist](
      transaction.amounts,
      amt => PropertyValidator.exists(amt.categoryId, categoryRepository.get)
    )

  private def validateAmounts[E <: ValidationError](
    amounts: Seq[Amount],
    validator: Amount => EitherT[F, E, Unit]
  ): EitherT[F, E, Unit] =
    amounts match {
      case Nil => EitherT.rightT(())
      case x :: y => validator(x) flatMap {
        _ => validateAmounts(y, validator)
      }
    }
}
