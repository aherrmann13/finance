package com.finance.business.validation

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.category._
import com.finance.business.model.types.{Description, Id, ModelName, Name, Usd}
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockCategoryRepository = stub[CategoryRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]

  private val categoryValidationInterpreter = new CategoryValidationInterpreter[IdMonad](
    mockCategoryRepository,
    mockTransactionRepository
  )

  private val categoryName = ModelName("Category")
  private val fakeCategoryWithId =
    Category(Some(Id(1)), Some(Id(5)), Name("name"), Description("desc"), Always, Seq.empty)
  private val fakeCategoryWithNoId = fakeCategoryWithId.copy(id = None)

  "idIsNone" - {
    "should return Left(IdMustBeNone) when id is Some" in {
      categoryValidationInterpreter.idIsNone(fakeCategoryWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](IdMustBeNone(categoryName)).value
    }
    "should return Right(()) when id is None" in {
      categoryValidationInterpreter.idIsNone(fakeCategoryWithNoId).value shouldEqual
        EitherT.rightT[IdMonad, IdMustBeNone](()).value
    }
  }
  "exists" - {
    "should return Left(DoesNotExist) when id is None" in {
      categoryValidationInterpreter.exists(fakeCategoryWithNoId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(categoryName)).value
    }
    "should return Left(DoesNotExist) when repository does not contain Account" in {
      (mockCategoryRepository get _).when(fakeCategoryWithId.id.get).returns(None.pure[IdMonad])
      categoryValidationInterpreter.exists(fakeCategoryWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(categoryName, fakeCategoryWithId.id)).value
    }
    "should return Right(()) when repository contains Account" in {
      (mockCategoryRepository get _).when(fakeCategoryWithId.id.get).returns(Some(fakeCategoryWithId).pure[IdMonad])
      categoryValidationInterpreter.exists(fakeCategoryWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "parentExists" - {
    "should return Left(DoesNotExist) when id does not exist" in {
      (mockCategoryRepository get _).when(fakeCategoryWithId.parentId.get).returns(None.pure[IdMonad])
      categoryValidationInterpreter.parentExists(fakeCategoryWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(categoryName, fakeCategoryWithId.parentId.get)).value
    }
    "should return Right(()) when id exists" in {
      (mockCategoryRepository get _).when(fakeCategoryWithId.parentId.get).returns(Some(fakeCategoryWithId))
      categoryValidationInterpreter.parentExists(fakeCategoryWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "withinParentTimePeriod" - {
    "should return Left(CategoryNotWithinParentTimePeriod) when not in parent time period" in {
      val parent = fakeCategoryWithId.copy(id = fakeCategoryWithId.parentId, effectiveTime = Single(2))
      (mockCategoryRepository get _).when(parent.id.get).returns(Some(parent))

      categoryValidationInterpreter.withinParentTimePeriod(fakeCategoryWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](
            CategoryNotWithinParentTimePeriod(fakeCategoryWithId.effectiveTime, parent.effectiveTime)
          ).value
    }
    "should return Right(()) when in parent time period" in {
      val parent = fakeCategoryWithId.copy(id = fakeCategoryWithId.parentId, effectiveTime = Always)
      (mockCategoryRepository get _).when(parent.id.get).returns(Some(parent).pure[IdMonad])

      categoryValidationInterpreter.withinParentTimePeriod(fakeCategoryWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
    "should return Right(()) when parent does not exist" in {
      (mockCategoryRepository get _).when(fakeCategoryWithId.parentId.get).returns(None.pure[IdMonad])
      categoryValidationInterpreter.withinParentTimePeriod(fakeCategoryWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
    "should return Right(()) when parentId is None" in {
      categoryValidationInterpreter.withinParentTimePeriod(fakeCategoryWithId.copy(parentId = None)).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "nameIsValid" - {
    "should return Left(NameIsTooLong) when name is too long" in {
      val name = Name((0 to 128).map(_ => "a").fold("")(_ + _))
      categoryValidationInterpreter.nameIsValid(fakeCategoryWithId.copy(name = name)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](NameTooLong(categoryName, name)).value
    }
    "should return Right(()) when name is correct length" in {
      val name = Name((0 to 127).map(_ => "a").fold("")(_ + _))
      categoryValidationInterpreter.nameIsValid(fakeCategoryWithId.copy(name = name)).value shouldEqual
        EitherT.rightT[IdMonad, NameTooLong](()).value
    }
  }
  "descriptionIsValid" - {
    "should return Left(DescriptionIsTooLong) when description is too long" in {
      val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
      categoryValidationInterpreter.descriptionIsValid(fakeCategoryWithId.copy(description = desc)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DescriptionTooLong(categoryName, desc)).value
    }
    "should return Right(()) when description is correct length" in {
      val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
      categoryValidationInterpreter.descriptionIsValid(fakeCategoryWithId.copy(description = desc)).value shouldEqual
        EitherT.rightT[IdMonad, DescriptionTooLong](()).value
    }
  }
  "budgetWithinCategoryTime" - {
    "should return Left(BudgetPeriodNotInEffectiveTime) for first budget not in category time period" in {
      val budget0 = Budget(Range(4, 5), Usd(60))
      val budget1 = Budget(Range(6, 8), Usd(60))
      val budget2 = Budget(Always, Usd(60))
      val cat = fakeCategoryWithId.copy(effectiveTime = Range(3, 7), budget = Seq(budget0, budget1, budget2))
      categoryValidationInterpreter.budgetWithinCategoryTime(cat).value shouldEqual
        EitherT.leftT[IdMonad, Unit](BudgetPeriodNotInEffectiveTime(budget1.effectiveTime, cat.effectiveTime)).value
    }
    "should return Right(()) when all budgets in category time period" in {
      val budget0 = Budget(Range(4, 5), Usd(60))
      val budget1 = Budget(Range(6, 8), Usd(60))
      val budget2 = Budget(Always, Usd(60))
      val cat = fakeCategoryWithId.copy(effectiveTime = Always, budget = Seq(budget0, budget1, budget2))
      categoryValidationInterpreter.budgetWithinCategoryTime(cat).value shouldEqual
        EitherT.rightT[IdMonad, BudgetPeriodNotInEffectiveTime](()).value
    }
  }
  "hasNoTransactions" - {
    "should return Left(HasTransactions) if there are transactions with account id" in {
      (mockTransactionRepository anyWithCategoryId _).when(fakeCategoryWithId.id.get).returns(true.pure[IdMonad])
      categoryValidationInterpreter.hasNoTransactions(fakeCategoryWithId.id.get).value shouldEqual
        EitherT.leftT[IdMonad, Unit](HasTransactions(categoryName)).value
    }
    "should return Right(()) if there are no transactions with no account id" in {
      (mockTransactionRepository anyWithCategoryId _).when(fakeCategoryWithId.id.get).returns(false.pure[IdMonad])
      categoryValidationInterpreter.hasNoTransactions(fakeCategoryWithId.id.get).value shouldEqual
        EitherT.rightT[IdMonad, HasTransactions](()).value
    }
  }
}
