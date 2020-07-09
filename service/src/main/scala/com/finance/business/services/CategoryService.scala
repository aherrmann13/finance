package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.implicits._
import com.finance.business.model.category.{Category, CategoryAmountSpent}
import com.finance.business.model.transaction.CategoryAmount
import com.finance.business.model.types.{DateRange, Id}
import com.finance.business.operations.CategoryOps._
import com.finance.business.operations.TransactionOps._
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors.ValidationError

object CategoryService {
  private def widenRange(categories: Seq[Category], range: DateRange): Option[DateRange] = {
    val ranges = overlappingDateRanges(categories, range)
    Option.unless(ranges.isEmpty)(DateRange(ranges.map(_.start).min, ranges.map(_.end).max))
  }

  private def overlappingDateRanges(categories: Seq[Category], range: DateRange): Seq[DateRange] =
    categories flatMap {
      _.budget
    } flatMap {
      _.effectiveTime
    } filter {
      _ overlaps range
    }

}

class CategoryService[F[_]: Monad](
  validator: CategoryValidationAlgebra[F],
  repository: CategoryRepository[F],
  transactionRepository: TransactionRepository[F]
) extends CommandService[F, Category]
    with QueryService[F, Category] {

  import CategoryService._

  override def create(model: Category): EitherT[F, ValidationError, Category] =
    for {
      _ <- validator idIsNone model
      _ <- validator parentExists model
      _ <- validator withinParentTimePeriod model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      _ <- validator budgetWithinCategoryTime model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Category): EitherT[F, ValidationError, Category] =
    for {
      _ <- validator exists model
      _ <- validator parentExists model
      _ <- validator withinParentTimePeriod model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      _ <- validator budgetWithinCategoryTime model
      _ <- validator transactionsWithinBudgetTime model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    for {
      _ <- validator hasNoTransactions id
      deleted <- EitherT.liftF(repository delete id)
    } yield deleted

  override def get(id: Id): OptionT[F, Category] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Category]] = repository.getMany(ids)

  override def getAll: F[Seq[Category]] = repository.getAll

  def getAmountSpentInRange(range: DateRange): F[Seq[CategoryAmountSpent]] =
    for {
      categories <- getAll
      wideRange = widenRange(categories, range)
      transactions <- wideRange.map {
        transactionRepository.getCategoryAmountsInRange
      } getOrElse Seq.empty[CategoryAmount].pure[F]
    } yield transactions.categoryValues(range, categories)
}
