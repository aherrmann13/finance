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

  override def parentExists(id: Id): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(id, categoryRepository.get)

  override def withinParentTimePeriod(id: Id, category: Category): EitherT[F, CategoryNotWithinParentTimePeriod, Unit] =
    EitherT {
      categoryRepository.get(id) map { cat =>
        cat map { parent =>
          Either.cond(
            category.effectiveTime within parent.effectiveTime,
            (),
            CategoryNotWithinParentTimePeriod(category.effectiveTime, parent.effectiveTime))
        } getOrElse Either.right[CategoryNotWithinParentTimePeriod, Unit](())
      }
    }

  override def nameIsValid(category: Category): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(category)

  override def descriptionIsValid(category: Category): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(category)

  override def budgetWithinCategoryTime(category: Category): EitherT[F, BudgetPeriodNotInEffectiveTime, Unit] =
    withinCategoryTime(category.budget, category.effectiveTime)

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    EitherT {
      transactionRepository.anyWithCategoryId(id) map { exists =>
        Either.cond(!exists, (), HasTransactions(Name))
      }
    }

  private def withinCategoryTime(
      budget: Seq[Budget],
      time: EffectiveTime
  ): EitherT[F, BudgetPeriodNotInEffectiveTime, Unit] =
    budget match {
      case Nil => EitherT.rightT(())
      case x :: y =>
        EitherT.cond[F](x.effectiveTime within time, (), BudgetPeriodNotInEffectiveTime(x.effectiveTime, time)) flatMap {
          _ =>
            withinCategoryTime(y, time)
        }
    }
}
