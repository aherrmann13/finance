package com.finance.business.services

import cats.data.EitherT
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.category.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent}
import com.finance.business.model.transaction.{CategoryAmount, Transaction}
import com.finance.business.model.types.{DateRange, Description, Id, ModelName, Name, Usd}
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[CategoryValidationAlgebra[IdMonad]]
  private val mockRepository = mock[CategoryRepository[IdMonad]]
  private val mockTransactionRepository = mock[TransactionRepository[IdMonad]]

  private val service = new CategoryService(mockValidationAlgebra, mockRepository, mockTransactionRepository)

  private val categoryId = Id(5)
  private val category = Category(Some(categoryId), None, Name("Name"), Description("Description"), Seq.empty, Seq.empty)

  "CategoryService" - {
    "create" - {
      "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Category")))
        (mockValidationAlgebra idIsNone _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra parentExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "returns Left(CategoryEffectiveTimeNotWithinParent) from validation algebra withinParentTimePeriod" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](CategoryEffectiveTimeNotWithinParent(Seq.empty, Seq.empty))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "returns Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Category"), Name("Name")))
        (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns returnVal
        (mockRepository create _) expects category never

        service.create(category) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
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
      "returns Left(BudgetEffectiveTimeNotWithinCategory) from validation algebra budgetWithinCategoryTime" in {
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
      "returns Right(()) and saves model when validation passes" in {
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
      "returns Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra exists _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra parentExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category")))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "returns Left(CategoryEffectiveTimeNotWithinParent) from validation algebra withinParentTimePeriod" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](CategoryEffectiveTimeNotWithinParent(Seq.empty, Seq.empty))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "returns Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Category"), Name("Name")))
        (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra withinParentTimePeriod _) when category returns
          EitherT.rightT[IdMonad, CategoryEffectiveTimeNotWithinParent](())
        (mockValidationAlgebra nameIsValid _) when category returns returnVal
        (mockRepository update _) expects category never

        service.update(category) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
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
      "returns Left(BudgetEffectiveTimeNotWithinCategory) from validation algebra budgetWithinCategoryTime" in {
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
      "returns Left(TransactionNotWithinBudgetEffectiveTime) from validation algebra transactionsWithinCategoryTime" in {
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
      "returns Right(()) and updates model when validation passes" in {
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
      "returns Left(HasTransactions) from validation algebra hasNoTransactions" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Category")))
        (mockValidationAlgebra hasNoTransactions _) when categoryId returns returnVal
        (mockRepository delete _) expects categoryId never

        service.delete(categoryId) shouldEqual returnVal
      }
      "returns Right(()) and deletes when validation passes" in {
        (mockValidationAlgebra hasNoTransactions _) when categoryId returns EitherT.rightT[IdMonad, HasTransactions](())
        (mockRepository delete _) expects categoryId returns ().pure[IdMonad]

        service.delete(categoryId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "returns repository get" in {
        (mockRepository get _) expects categoryId returns Some(category).pure[IdMonad]

        service.get(categoryId) shouldEqual Some(category)
      }
    }
    "getMany" - {
      "returns repository getMany" in {
        (mockRepository getMany _) expects Seq(categoryId, Id(categoryId.value + 1)) returns
          Seq(category, category).pure[IdMonad]

        service.getMany(Seq(categoryId, Id(categoryId.value + 1))) shouldEqual Seq(category, category)
      }
    }
    "getAll" - {
      "returns repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(category, category).pure[IdMonad])

        service.getAll shouldEqual Seq(category, category)
      }
    }
    "getAmountSpentInRange" - {
      "requests transactions with wider range than passed in" in {
        val budget0 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-01-01"), DateTime.parse("2020-01-31")),
            DateRange(DateTime.parse("2020-03-01"), DateTime.parse("2020-03-31"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-02-01"), DateTime.parse("2020-02-28")),
            DateRange(DateTime.parse("2020-04-01"), DateTime.parse("2020-04-30"))
          ),
          Usd(30)
        )

        (mockRepository.getAll _).expects().returns(Seq(category.copy(budget = Seq(budget0, budget1))).pure[IdMonad])

        mockTransactionRepository.getCategoryAmountsInRange _ expects DateRange(
          budget1.effectiveTime.head.start, budget0.effectiveTime(1).end
        ) returns Seq.empty[CategoryAmount].pure[IdMonad]

        service.getAmountSpentInRange(
          DateRange(DateTime.parse("2020-02-15"), DateTime.parse("2020-03-15"))
        )
      }
      "does not requests transactions if no valid date range" in {
        val budget0 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-01-01"), DateTime.parse("2020-01-31")),
            DateRange(DateTime.parse("2020-03-01"), DateTime.parse("2020-03-31"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-02-01"), DateTime.parse("2020-02-28")),
            DateRange(DateTime.parse("2020-04-01"), DateTime.parse("2020-04-30"))
          ),
          Usd(30)
        )

        (mockRepository.getAll _).expects().returns(Seq(category.copy(budget = Seq(budget0, budget1))).pure[IdMonad])

        mockTransactionRepository.getCategoryAmountsInRange _ expects * never

        service.getAmountSpentInRange(
          DateRange(DateTime.parse("2019-02-15"), DateTime.parse("2019-03-15"))
        )
      }
      "returns all categories as category values with amount totals" in {
        val budget0 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-01-01"), DateTime.parse("2020-01-31"))
          ),
          Usd(30)
        )
        val budget1 = Budget(
          Seq(
            DateRange(DateTime.parse("2020-02-01"), DateTime.parse("2020-02-28"))
          ),
          Usd(30)
        )

        val cat = category.copy(budget = Seq(budget0, budget1))

        (mockRepository.getAll _).expects().returns(Seq(cat).pure[IdMonad])

        val amt0 = CategoryAmount(cat.id.get, Id(6), Usd(10), Description("desc"), DateTime.parse("2020-01-13"))
        val amt1 = CategoryAmount(cat.id.get, Id(6), Usd(20), Description("desc"), DateTime.parse("2020-01-15"))
        val amt2 = CategoryAmount(cat.id.get, Id(6), Usd(30), Description("desc"), DateTime.parse("2020-02-15"))
        val amt3 = CategoryAmount(cat.id.get, Id(6), Usd(40), Description("desc"), DateTime.parse("2020-03-15"))

        mockTransactionRepository.getCategoryAmountsInRange _ expects DateRange(
          budget0.effectiveTime.head.start, budget1.effectiveTime.head.end
        ) returns Seq(amt0, amt1, amt2, amt3).pure[IdMonad]

        service.getAmountSpentInRange(
          DateRange(DateTime.parse("2020-01-14"), DateTime.parse("2020-02-17"))
        ) shouldEqual Seq(
          CategoryAmountSpent(cat, Seq(
            BudgetAmountSpent(budget0.effectiveTime, Usd(20), Usd(10), budget0.amount),
            BudgetAmountSpent(budget1.effectiveTime, Usd(30), Usd(0), budget1.amount)
          ))
        )
      }
    }
  }
}