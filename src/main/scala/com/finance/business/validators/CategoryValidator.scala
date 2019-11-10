package com.finance.business.validators

import cats.implicits._
import cats.{Applicative, Monad}
import cats.data.EitherT
import com.finance.business.errors._
import com.finance.business.model.category.{Category, CategoryRepository, Range}
import com.finance.business.model.operations.Category._
import com.finance.business.model.transaction.TransactionRepository
import com.finance.business.validators.internal.PropertyValidators._

object CategoryValidator {
  def apply[F[_]: Applicative](
      categoryRepository: CategoryRepository[F],
      transactionRepository: TransactionRepository[F]) =
    new CategoryValidator[F](categoryRepository, transactionRepository)
}

class CategoryValidator[F[_]: Applicative](
    categoryRepository: CategoryRepository[F],
    transactionRepository: TransactionRepository[F]
) {
  def parentExists(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.parentId match {
        case Some(id) =>
          categoryRepository.get(category.userId, id).map {
            case Some(_) => Right(())
            case None => Left(ParentCategoryDoesNotExistError)
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def withinParentTimePeriod(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.parentId match {
        case Some(id) =>
          categoryRepository.get(category.userId, id).map {
            case Some(parent) =>
              if (category.effective.within(parent.effective)) Right(()) else Left(CategoryEffectiveTimeOutsideParent)
            case None => Right(())
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def parentIsNotLeaf(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.parentId match {
        case Some(id) =>
          categoryRepository.get(category.userId, id).map {
            case Some(parent) =>
              if (parent.isLeaf) Left(ParentCategoryCannotBeLeaf) else Right(())
            case None => Right(())
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def exists(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.id match {
        case Some(id) =>
          categoryRepository.get(category.userId, id).map {
            case Some(_) => Right(())
            case None => Left(CategoryDoesNotExistError)
          }
        case None => Either.left[BusinessError, Unit](CategoryDoesNotExistError).pure[F]
      }
    }

  def doesNotExist(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.id match {
        case Some(id) =>
          categoryRepository.get(category.userId, id).map {
            case Some(_) => Left(CategoryAlreadyExistsError)
            case None => Right(())
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def propertiesAreValid(category: Category)(implicit M: Monad[F]): EitherT[F, BusinessError, Unit] =
    for {
      _ <- validateName[F](category.name)
      _ <- validateDesc[F](category.description)
      _ <- validateTimePeriod(category)
    } yield Right(())

  def hasTransactions(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.id match {
        case Some(id) =>
          transactionRepository.anyWithCategoryId(category.userId, id).map {
            if (_) Left(ReferencedByTransactionError) else Right(())
          }
        case None => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  private def validateTimePeriod(category: Category): EitherT[F, BusinessError, Unit] =
    EitherT {
      category.effective match {
        case Range(from, to) if to < from =>
          Either.left[BusinessError, Unit](CategoryEffectiveTimeInvalidFormat).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }
}
