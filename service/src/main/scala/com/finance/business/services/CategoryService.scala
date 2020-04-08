package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.category.Category
import com.finance.business.model.types.Id
import com.finance.business.repository.CategoryRepository
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class CategoryService[F[_]: Monad](
    validator: CategoryValidationAlgebra[F],
    repository: CategoryRepository[F]
) extends CommandService[F, Category] {
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
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    for {
      _ <- validator hasNoTransactions id
      deleted <- EitherT.liftF(repository delete id)
    } yield deleted
}
