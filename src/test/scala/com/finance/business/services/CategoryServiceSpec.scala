package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import com.finance.business.common.{IdRepository, RelationValidator}
import com.finance.business.errors._
import com.finance.business.model.category.{Always, Budget, Category, CategoryRepository}
import com.finance.business.model.source.Source
import com.finance.business.model.transaction.TransactionRepository
import com.finance.business.validators.CategoryValidator
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class CategoryServiceSpec extends FreeSpec with Matchers with MockFactory {
  private val repository = stub[CategoryRepository[IO]]

  //https://github.com/paulbutcher/ScalaMock/issues/170
  class CategoryValidatorWithIO(
      sourceRepository: CategoryRepository[IO],
      transactionRepository: TransactionRepository[IO])
      extends CategoryValidator[IO](sourceRepository, transactionRepository)

  class RelationValidatorWithIO(repository: IdRepository[IO]) extends RelationValidator[IO](repository)

  private val validator = mock[CategoryValidatorWithIO]
  private val relationValidator = mock[RelationValidatorWithIO]

  private val service = new CategoryService(repository, validator, relationValidator)

  private val fakeCategory =
    Category(Option(1), 2, Option(3), "name", "description", Always, isLeaf = true, Seq(Budget(2, 20)))

  "Category service" - {
    "create" - {
      "should return Left(BusinessError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(BusinessError) error when user does not exist" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Left(BusinessError) error when category exists" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](CategoryAlreadyExistsError))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(CategoryAlreadyExistsError)
      }
      "should return Left(BusinessError) error when parent does not exist" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](ParentCategoryDoesNotExistError))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryDoesNotExistError)
      }
      "should return Left(BusinessError) error when parent is leaf" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](ParentCategoryCannotBeLeaf))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryCannotBeLeaf)
      }
      "should return Left(BusinessError) error when outside parent time period" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .withinParentTimePeriod(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](CategoryEffectiveTimeOutsideParent))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(CategoryEffectiveTimeOutsideParent)
      }
      "should return Right(Category) on success" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .withinParentTimePeriod(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.create _ when fakeCategory).returns(IO(fakeCategory))

        val result = service.create(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(fakeCategory)
      }
    }
    "update" - {
      "should return Left(BusinessError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(BusinessError) error when user does not exist" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Left(BusinessError) error when category does not" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](CategoryDoesNotExistError))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(CategoryDoesNotExistError)
      }
      "should return Left(BusinessError) error when parent does not exist" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](ParentCategoryDoesNotExistError))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryDoesNotExistError)
      }
      "should return Left(BusinessError) error when parent is leaf" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](ParentCategoryCannotBeLeaf))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryCannotBeLeaf)
      }
      "should return Left(BusinessError) error when outside parent time period" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .withinParentTimePeriod(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](CategoryEffectiveTimeOutsideParent))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(CategoryEffectiveTimeOutsideParent)
      }
      "should return Right(Category) on success" in {
        (validator
          .propertiesAreValid(_: Category)(_: Monad[IO]))
          .expects(fakeCategory, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentExists(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .parentIsNotLeaf(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .withinParentTimePeriod(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.update _ when fakeCategory).returns(IO(fakeCategory))

        val result = service.update(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(fakeCategory)
      }
    }
    "delete" - {
      "should return Left(ReferencedByTransactionError) if transaction exists" in {
        (validator
          .hasTransactions(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.leftT[IO, Unit](ReferencedByTransactionError))

        val result = service.delete(fakeCategory)

        (repository.delete _).verify(*, *) never

        result.value.unsafeRunSync shouldBe Left(ReferencedByTransactionError)
      }
      "should return Right(()) and delete source on success" in {
        (validator
          .hasTransactions(_: Category))
          .expects(fakeCategory)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.delete _).when(fakeCategory.userId, fakeCategory.id.get).returns(IO(()))

        val result = service.delete(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) and skip delete when source has no id" in {
        val result = service.delete(fakeCategory.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())

        (repository.delete _).verify(*, *) never
      }
    }
    "get" - {
      "should call get on repository" in {
        val userId = 5
        val id = 3
        service.get(userId, id)

        (repository.get _).verify(userId, id)
      }

      "should return results from repository" in {
        (repository.get _).when(fakeCategory.userId, fakeCategory.id.get).returns(IO(Some(fakeCategory)))

        val result = service.get(fakeCategory.userId, fakeCategory.id.get)
        result.unsafeRunSync shouldBe Option.apply(fakeCategory)
      }
    }
    "getMany" - {
      "should call getMany on repository" in {
        val userId = 4
        val ids = Seq(1, 2, 3, 4)
        service.getMany(userId, ids)

        (repository.getMany _).verify(userId, ids)
      }

      "should return results from repository" in {
        val sources = Seq(fakeCategory, fakeCategory.copy(id = Option.apply(5)))
        val ids = sources.flatMap(_.id)
        (repository.getMany _).when(fakeCategory.userId, ids).returns(IO(sources))

        val result = service.getMany(fakeCategory.userId, ids)
        result.unsafeRunSync shouldBe sources
      }
    }
    "getAll" - {
      "should call getAll on repository" in {
        val userId = 4
        service.getAll(userId)

        (repository.getAll _).verify(userId)
      }

      "should return results from repository" in {
        val sources = Seq(fakeCategory, fakeCategory.copy(id = Option.apply(5)))
        (repository.getAll _).when(fakeCategory.userId).returns(IO(sources))

        val result = service.getAll(fakeCategory.userId)
        result.unsafeRunSync shouldBe sources
      }
    }
  }
}
