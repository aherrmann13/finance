package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import com.finance.business.common.{RelationValidator, Service}
import com.finance.business.errors.BusinessError
import com.finance.business.model.category.{Category, CategoryRepository}
import com.finance.business.validators.CategoryValidator

class CategoryService[F[_]](
    repository: CategoryRepository[F],
    validator: CategoryValidator[F],
    relationValidator: RelationValidator[F]
) extends Service[F, Category] {
  override def create(category: Category)(implicit M: Monad[F]): EitherT[F, BusinessError, Category] =
    for {
      _ <- validator.propertiesAreValid(category)
      _ <- relationValidator.userExists(category)
      _ <- validator.doesNotExist(category)
      _ <- validator.parentExists(category)
      _ <- validator.parentIsNotLeaf(category)
      _ <- validator.withinParentTimePeriod(category)
      saved <- EitherT.liftF(repository.create(category))
    } yield saved

  override def update(category: Category)(implicit M: Monad[F]): EitherT[F, BusinessError, Category] =
    for {
      _ <- validator.propertiesAreValid(category)
      _ <- relationValidator.userExists(category)
      _ <- validator.exists(category)
      _ <- validator.parentExists(category)
      _ <- validator.parentIsNotLeaf(category)
      _ <- validator.withinParentTimePeriod(category)
      saved <- EitherT.liftF(repository.update(category))
    } yield saved

  // TODO: clean this up
  override def delete(category: Category)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    category.id match {
      case Some(id) =>
        for {
          _ <- validator.hasTransactions(category)
          result <- EitherT.liftF(repository.delete(category.userId, id))
        } yield result
      case None => EitherT.rightT[F, BusinessError](())
    }

  def get(userId: Int, id: Int): F[Option[Category]] = repository.get(userId, id)

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Category]] = repository.getMany(userId, id)

  def getAll(userId: Int): F[Seq[Category]] = repository.getAll(userId)
}
