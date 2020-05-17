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

class CategoryValidationInterpreter[F[_] : Monad](
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

  override def withinParentTimePeriod(category: Category): EitherT[F, CategoryEffectiveTimeNotWithinParent, Unit] =
    EitherT {
      category.parentId map {
        categoryRepository.get(_) map {
          _ map { parent =>
            Either.cond(
              category.effectiveTime.forall(_ within parent.effectiveTime),
              (),
              CategoryEffectiveTimeNotWithinParent(category.effectiveTime, parent.effectiveTime))
          } getOrElse Either.right[CategoryEffectiveTimeNotWithinParent, Unit](())
        }
      } getOrElse Either.right[CategoryEffectiveTimeNotWithinParent, Unit](()).pure[F]
    }

  override def nameIsValid(category: Category): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(category)

  override def descriptionIsValid(category: Category): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(category)

  override def budgetWithinCategoryTime(category: Category): EitherT[F, BudgetEffectiveTimeNotWithinCategory, Unit] =
    category.budget.toList.traverse { budget =>
      EitherT.cond[F](
        budget.effectiveTime.forall(x => x within category.effectiveTime),
        (),
        BudgetEffectiveTimeNotWithinCategory(budget.effectiveTime, category.effectiveTime))
    }.as(())

  override def transactionsWithinBudgetTime(
    category: Category
  ): EitherT[F, TransactionNotWithinBudgetEffectiveTime, Unit] =
    EitherT {
      category.id map {
        transactionRepository
          .anyOutsideRanges(_, category.budget.flatMap(_.effectiveTime)).map { exists =>
            Either.cond(!exists, (), TransactionNotWithinBudgetEffectiveTime(category.budget.flatMap(_.effectiveTime)))
          }
      } getOrElse Either.right[TransactionNotWithinBudgetEffectiveTime, Unit](()).pure[F]
    }

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    EitherT {
      transactionRepository.anyWithCategoryId(id) map { exists =>
        Either.cond(!exists, (), HasTransactions(Name))
      }
    }
}
