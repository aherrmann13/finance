package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.category._
import com.finance.business.model.category.implicits._
import com.finance.business.model.types.{Id, ModelName}
import com.finance.business.operations.CategoryOps._
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.validation.errors._

object CategoryValidationInterpreter {
  private val Name = ModelName("Category")
}

class CategoryValidationInterpreter[F[_]: Monad](
    categoryRepository: CategoryRepository[F],
    transactionRepository: TransactionRepository[F]
) extends CategoryValidationAlgebra[F] {
  import CategoryValidationInterpreter._

  override def idIsNone(category: Category): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(category)

  override def exists(category: Category): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(category.id, categoryRepository.get)

  override def parentExists(category: Category): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(category.parentId, categoryRepository.get)

  override def withinParentTimePeriod(category: Category): EitherT[F, CategoryNotWithinParentTimePeriod, Unit] =
    EitherT {
      category.parentId map {
        categoryRepository.get(_) map {
          _ map { parent =>
            Either.cond(
              category.effectiveTime within parent.effectiveTime,
              (),
              CategoryNotWithinParentTimePeriod(category.effectiveTime, parent.effectiveTime))
          } getOrElse Either.right[CategoryNotWithinParentTimePeriod, Unit](())
        }
      } getOrElse Either.right[CategoryNotWithinParentTimePeriod, Unit](()).pure[F]
    }

  override def nameIsValid(category: Category): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(category)

  override def descriptionIsValid(category: Category): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(category)

  override def budgetWithinCategoryTime(category: Category): EitherT[F, BudgetPeriodNotInEffectiveTime, Unit] =
    category.budget.toList.traverse { budget =>
      EitherT.cond[F](
        budget.effectiveTime within category.effectiveTime,
        (),
        BudgetPeriodNotInEffectiveTime(budget.effectiveTime, category.effectiveTime))
    } map( _ => ())

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    EitherT {
      transactionRepository.anyWithCategoryId(id) map { exists =>
        Either.cond(!exists, (), HasTransactions(Name))
      }
    }
}
