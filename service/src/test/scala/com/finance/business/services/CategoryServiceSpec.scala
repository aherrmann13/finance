package com.finance.business.services

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.category.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent}
import com.finance.business.model.transaction.CategoryAmount
import com.finance.business.model.types._
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[CategoryValidationAlgebra[IdMonad]]
  private val mockRepository = mock[CategoryRepository[IdMonad]]
  private val mockTransactionRepository = mock[TransactionRepository[IdMonad]]

  private val service = new CategoryService(mockValidationAlgebra, mockRepository, mockTransactionRepository)

  private val categoryId = Id(5)
  private val category =
    Category(Some(categoryId), None, Name("Name"), Description("Description"), Seq.empty, Seq.empty)

  "CategoryService" - {
    "create" - {
      "should return Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Category")))
        (mockValidationAlgebra idIsNone _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra parentExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Left(CategoryEffectiveTimeNotWithinParent) from validation algebra withinParentTimePeriod" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](CategoryEffectiveTimeNotWithinParent(Seq.empty, Seq.empty))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Category"), Name("Name")))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Category"), Description("Desc")))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Left(BudgetEffectiveTimeNotWithinCategory) from validation algebra budgetWithinCategoryTime" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](BudgetEffectiveTimeNotWithinCategory(Seq.empty, Seq.empty))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra budgetWithinCategoryTime _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
          EitherT.rightT[IdMonad, BudgetEffectiveTimeNotWithinCategory](())
        (mockRepository create _) expects category returns category.pure[IdMonad]

        service.create(category) shouldEqual EitherT.rightT[IdMonad, ValidationError](category)
      }
    }
    "update" - {
      "should return Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra exists _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra parentExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(CategoryEffectiveTimeNotWithinParent) from validation algebra withinParentTimePeriod" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](CategoryEffectiveTimeNotWithinParent(Seq.empty, Seq.empty))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Category"), Name("Name")))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Category"), Description("Desc")))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(BudgetEffectiveTimeNotWithinCategory) from validation algebra budgetWithinCategoryTime" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](BudgetEffectiveTimeNotWithinCategory(Seq.empty, Seq.empty))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra budgetWithinCategoryTime _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Left(TransactionNotWithinBudgetEffectiveTime) from validation algebra transactionsWithinCategoryTime" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](TransactionNotWithinBudgetEffectiveTime(Seq.empty))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
          EitherT.rightT[IdMonad, BudgetEffectiveTimeNotWithinCategory](())
        (mockValidationAlgebra transactionsWithinBudgetTime _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "should return Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when category returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
          EitherT.rightT[IdMonad, BudgetEffectiveTimeNotWithinCategory](())
        (mockValidationAlgebra transactionsWithinBudgetTime _) when category returns
          EitherT.rightT[IdMonad, TransactionNotWithinBudgetEffectiveTime](())
        (mockRepository update _) expects category returns category.pure[IdMonad]

        service.update(category) shouldEqual EitherT.rightT[IdMonad, ValidationError](category)
      }
    }
    "delete" - {
      "should return Left(HasTransactions) from validation algebra hasNoTransactions" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Category")))
        (mockValidationAlgebra hasNoTransactions _) when categoryId returns returnVal
        (mockRepository delete _) expects categoryId never

        service.delete(categoryId) shouldEqual returnVal
      }
      "should return Right(()) and deletes when validation passes" in {
        (mockValidationAlgebra hasNoTransactions _) when categoryId returns EitherT.rightT[IdMonad, HasTransactions](())
        (mockRepository delete _) expects categoryId returns ().pure[IdMonad]

        service.delete(categoryId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "should return repository get" in {
        (mockRepository get _) expects categoryId returns OptionT.pure(category)

        service.get(categoryId).value shouldEqual Some(category)
      }
    }
    "getMany" - {
      "should return repository getMany" in {
        (mockRepository getMany _) expects Seq(categoryId, Id(categoryId.value + 1)) returns
          Seq(category, category).pure[IdMonad]

        service.getMany(Seq(categoryId, Id(categoryId.value + 1))) shouldEqual Seq(category, category)
      }
    }
    "getAll" - {
      "should return repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(category, category).pure[IdMonad])

        service.getAll shouldEqual Seq(category, category)
      }
    }
    "getAmountSpentInRange" - {
      "requests transactions with wider range than passed in" in {
        val budget0 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-01-31T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2020-03-01T00:00:00Z"), OffsetDateTime.parse("2020-03-31T00:00:00Z"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2020-02-01T00:00:00Z"), OffsetDateTime.parse("2020-02-28T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2020-04-01T00:00:00Z"), OffsetDateTime.parse("2020-04-30T00:00:00Z"))
          ),
          Usd(30)
        )

        (mockRepository.getAll _).expects().returns(Seq(category.copy(budget = Seq(budget0, budget1))).pure[IdMonad])

        mockTransactionRepository.getCategoryAmountsInRange _ expects DateRange(
          budget1.effectiveTime.head.start,
          budget0.effectiveTime(1).end
        ) returns Seq.empty[CategoryAmount].pure[IdMonad]

        service.getAmountSpentInRange(
          DateRange(OffsetDateTime.parse("2020-02-15T00:00:00Z"), OffsetDateTime.parse("2020-03-15T00:00:00Z"))
        )
      }
      "does not requests transactions if no valid date range" in {
        val budget0 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-01-31T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2020-03-01T00:00:00Z"), OffsetDateTime.parse("2020-03-31T00:00:00Z"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2020-02-01T00:00:00Z"), OffsetDateTime.parse("2020-02-28T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2020-04-01T00:00:00Z"), OffsetDateTime.parse("2020-04-30T00:00:00Z"))
          ),
          Usd(30)
        )

        (mockRepository.getAll _).expects().returns(Seq(category.copy(budget = Seq(budget0, budget1))).pure[IdMonad])

        mockTransactionRepository.getCategoryAmountsInRange _ expects * never

        service.getAmountSpentInRange(
          DateRange(OffsetDateTime.parse("2019-02-15T00:00:00Z"), OffsetDateTime.parse("2019-03-15T00:00:00Z"))
        )
      }
      "should return all categories as category values with amount totals" in {
        val budget0 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-01-31T00:00:00Z"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(DateRange(OffsetDateTime.parse("2020-02-01T00:00:00Z"), OffsetDateTime.parse("2020-02-28T00:00:00Z"))),
          Usd(30)
        )

        val cat = category.copy(budget = Seq(budget0, budget1))

        (mockRepository.getAll _).expects().returns(Seq(cat).pure[IdMonad])

        val amt0 = CategoryAmount(
          cat.id.get,
          Id(6),
          Usd(10),
          Description("desc"),
          OffsetDateTime.parse("2020-01-13T00:00:00Z")
        )
        val amt1 = amt0.copy(amount = Usd(20), reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
        val amt2 = amt0.copy(amount = Usd(30), reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))
        val amt3 = amt0.copy(amount = Usd(40), reportingDate = OffsetDateTime.parse("2020-03-15T00:00:00Z"))

        mockTransactionRepository.getCategoryAmountsInRange _ expects DateRange(
          budget0.effectiveTime.head.start,
          budget1.effectiveTime.head.end
        ) returns Seq(amt0, amt1, amt2, amt3).pure[IdMonad]

        service.getAmountSpentInRange(
          DateRange(OffsetDateTime.parse("2020-01-14T00:00:00Z"), OffsetDateTime.parse("2020-02-17T00:00:00Z"))
        ) shouldEqual Seq(
          CategoryAmountSpent(
            cat,
            Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(20), Usd(10), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(30), Usd(0), budget1.amount)
            )
          )
        )
      }
    }
  }
}
