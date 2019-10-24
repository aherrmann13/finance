package com.finance.business.validators

import cats.effect.IO
import com.finance.business.common.Constants._
import com.finance.business.errors._
import com.finance.business.model.category._
import com.finance.business.model.transaction.TransactionRepository
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class CategoryValidatorSpec extends FreeSpec with Matchers with MockFactory {
  private val categoryRepository = stub[CategoryRepository[IO]]
  private val transactionRepository = stub[TransactionRepository[IO]]
  private val validator = CategoryValidator(categoryRepository, transactionRepository)

  private val fakeCategory =
    Category(2, Option(3), "name", "description", Always, isLeaf = true, Seq(Budget(2, 20)))

  "CategoryValidator method" - {
    "parentExists" - {
      "should return Left(ParentCategoryDoesNotExistError) when parent category does not exist" in {
        (categoryRepository.get _).when(fakeCategory.userId, fakeCategory.parentId.get).returns(IO(Option.empty))

        val result = validator.parentExists(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryDoesNotExistError)
      }
      "should return Right(()) when parent category exists" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(Option(fakeCategory)))

        val result = validator.parentExists(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when parent category is None" in {
        val result = validator.parentExists(fakeCategory.copy(parentId = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
    }
    "withinParentTimePeriod" - {
      "should return Left(CategoryEffectiveTimeOutsideParent) when outside parent category period" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(Option(fakeCategory.copy(effective = Single(2)))))

        val result = validator.withinParentTimePeriod(fakeCategory.copy(effective = Always))

        result.value.unsafeRunSync shouldBe Left(CategoryEffectiveTimeOutsideParent)
      }
      "should return Right(()) when inside parent category period" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(Option(fakeCategory)))

        val result = validator.withinParentTimePeriod(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when parent does not exist" in {
        (categoryRepository.get _).when(fakeCategory.userId, fakeCategory.parentId.get).returns(IO(Option.empty))

        val result = validator.withinParentTimePeriod(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when parent id is None" in {
        val result = validator.withinParentTimePeriod(fakeCategory.copy(parentId = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "parentIsNotLeaf" - {
      "should return Left(ParentCategoryCannotBeLeaf) when parent is leaf" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(Option(fakeCategory.copy(isLeaf = true))))

        val result = validator.parentIsNotLeaf(fakeCategory)

        result.value.unsafeRunSync shouldBe Left(ParentCategoryCannotBeLeaf)
      }
      "should return Right(()) when parent is not leaf" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(Option(fakeCategory.copy(isLeaf = false))))

        val result = validator.parentIsNotLeaf(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) when parent does not exist" in {
        (categoryRepository.get _)
          .when(fakeCategory.userId, fakeCategory.parentId.get)
          .returns(IO(None))

        val result = validator.parentIsNotLeaf(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) when parent id is null" in {
        val result = validator.parentIsNotLeaf(fakeCategory.copy(parentId = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "exists" - {
      "should return Right(()) when category exists" in {
        val id = 2
        (categoryRepository.get _)
          .when(fakeCategory.userId, id)
          .returns(IO(Option(fakeCategory)))

        val result = validator.exists(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Right(())

      }
      "should return Left(CategoryDoesNotExistError) when category does not exist" in {
        val id = 2
        (categoryRepository.get _)
          .when(fakeCategory.userId, id)
          .returns(IO(None))

        val result = validator.exists(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Left(CategoryDoesNotExistError)
      }
      "should return Left(CategoryDoesNotExistError) when category id is None" in {
        val result = validator.exists(fakeCategory.copy(id = None))

        result.value.unsafeRunSync shouldBe Left(CategoryDoesNotExistError)
      }
    }

    "doesNotExist" - {
      "should return Left(CategoryAlreadyExistsError) when category exists" in {
        val id = 2
        (categoryRepository.get _)
          .when(fakeCategory.userId, id)
          .returns(IO(Option(fakeCategory)))

        val result = validator.doesNotExist(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Left(CategoryAlreadyExistsError)

      }
      "should return Right(()) when category does not exist" in {
        val id = 2
        (categoryRepository.get _)
          .when(fakeCategory.userId, id)
          .returns(IO(None))

        val result = validator.doesNotExist(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) when category id is None" in {
        val result = validator.doesNotExist(fakeCategory.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "propertiesAreValid" - {
      "should return Right(()) when all properties are valid" in {
        val result = validator.propertiesAreValid(fakeCategory)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(NameMustBeDefinedError) when name null" in {
        val result = validator.propertiesAreValid(fakeCategory.copy(name = null))

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(NameExceedsMaxLengthError) when name too long" in {
        val name = (0 until (MaxNameLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val result = validator.propertiesAreValid(fakeCategory.copy(name = name))

        result.value.unsafeRunSync shouldBe Left(NameExceedsMaxLengthError)
      }
      "should return Left(DescriptionMustBeDefinedError) when description null" in {
        val result = validator.propertiesAreValid(fakeCategory.copy(description = null))

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }
      "should return Left(DescriptionExceedsMaxLengthError) when description too long" in {
        val desc = (0 until (MaxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val result = validator.propertiesAreValid(fakeCategory.copy(description = desc))

        result.value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }
    }

    "hasTransactions" - {
      "should return Right(()) when category has None as id" in {
        val result = validator.hasTransactions(fakeCategory.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when category has no transactions" in {
        val id = 2
        (transactionRepository.anyWithCategoryId _)
          .when(fakeCategory.userId, id)
          .returns(IO(false))

        val result = validator.hasTransactions(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(ReferencedByTransactionError) when category has transactions" in {
        val id = 2
        (transactionRepository.anyWithCategoryId _)
          .when(fakeCategory.userId, id)
          .returns(IO(true))

        val result = validator.hasTransactions(fakeCategory.copy(id = Some(id)))

        result.value.unsafeRunSync shouldBe Left(ReferencedByTransactionError)
      }
    }
  }
}
