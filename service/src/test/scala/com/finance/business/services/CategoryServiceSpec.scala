package com.finance.business.services

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.category.{Always, Category}
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.CategoryRepository
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[CategoryValidationAlgebra[IdMonad]]
  private val mockRepository = mock[CategoryRepository[IdMonad]]

  private val service = new CategoryService(mockValidationAlgebra, mockRepository)

  private val categoryId = Id(5)
  private val category = Category(Some(categoryId), None, Name("Name"), Description("Description"), Always, Seq.empty)

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
    "returns Left(CategoryNotWithinParentTimePeriod) from validation algebra withinParentTimePeriod" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](CategoryNotWithinParentTimePeriod(Always, Always))
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
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns returnVal
      (mockRepository create _) expects category never

      service.create(category) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Category"), Description("Desc")))
      (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns returnVal
      (mockRepository create _) expects category never

      service.create(category) shouldEqual returnVal
    }
    "returns Left(BudgetPeriodNotInEffectiveTime) from validation algebra budgetWithinCategoryTime" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](BudgetPeriodNotInEffectiveTime(Always, Always))
      (mockValidationAlgebra idIsNone _) when category returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
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
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns
        EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
        EitherT.rightT[IdMonad, BudgetPeriodNotInEffectiveTime](())
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
    "returns Left(CategoryNotWithinParentTimePeriod) from validation algebra withinParentTimePeriod" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](CategoryNotWithinParentTimePeriod(Always, Always))
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
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns returnVal
      (mockRepository update _) expects category never

      service.update(category) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Category"), Description("Desc")))
      (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns returnVal
      (mockRepository update _) expects category never

      service.update(category) shouldEqual returnVal
    }
    "returns Left(BudgetPeriodNotInEffectiveTime) from validation algebra budgetWithinCategoryTime" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](BudgetPeriodNotInEffectiveTime(Always, Always))
      (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns
        EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra budgetWithinCategoryTime _) when category returns returnVal
      (mockRepository update _) expects category never

      service.update(category) shouldEqual returnVal
    }
    "returns Left(CategoryTransactionNotWithinEffectiveTime) from validation algebra transactionsWithinCategoryTime" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](CategoryTransactionNotWithinEffectiveTime(Always))
      (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns
        EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
        EitherT.rightT[IdMonad, BudgetPeriodNotInEffectiveTime](())
      (mockValidationAlgebra transactionsWithinCategoryTime _) when category returns returnVal
      (mockRepository update _) expects category never

      service.update(category) shouldEqual returnVal
    }
    "returns Right(()) and updates model when validation passes" in {
      (mockValidationAlgebra exists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra parentExists _) when category returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra withinParentTimePeriod _) when category returns
        EitherT.rightT[IdMonad, CategoryNotWithinParentTimePeriod](())
      (mockValidationAlgebra nameIsValid _) when category returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when category returns
        EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra budgetWithinCategoryTime _) when category returns
        EitherT.rightT[IdMonad, BudgetPeriodNotInEffectiveTime](())
      (mockValidationAlgebra transactionsWithinCategoryTime _) when category returns
        EitherT.rightT[IdMonad, CategoryTransactionNotWithinEffectiveTime](())
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
}
