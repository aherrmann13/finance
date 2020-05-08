package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.category.Category
import com.finance.business.model.types.Id
import com.finance.business.validation.errors._

trait CategoryValidationAlgebra[F[_]] {
  def idIsNone(category: Category): EitherT[F, IdMustBeNone, Unit]

  def exists(category: Category): EitherT[F, DoesNotExist, Unit]

  def parentExists(category: Category): EitherT[F, DoesNotExist, Unit]

  def withinParentTimePeriod(category: Category): EitherT[F, CategoryNotWithinParentTimePeriod, Unit]

  def nameIsValid(category: Category): EitherT[F, NameTooLong, Unit]

  def descriptionIsValid(category: Category): EitherT[F, DescriptionTooLong, Unit]

  def budgetWithinCategoryTime(category: Category): EitherT[F, BudgetPeriodNotInEffectiveTime, Unit]

  def transactionsWithinCategoryTime(category: Category): EitherT[F, CategoryTransactionNotWithinEffectiveTime, Unit]

  def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit]
}
