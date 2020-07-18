package com.finance.service.handlers

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.category.{Category => CategoryModel, CategoryAmountSpent => CategoryAmountSpentModel}
import com.finance.business.model.types.{Description, Id, Name, DateRange => DateRangeModel}
import com.finance.business.repository.{CategoryRepository, TransactionRepository}
import com.finance.business.services.CategoryService
import com.finance.business.validation.CategoryValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.category._
import com.finance.service.endpoints.definitions.{
  AmountSpentInRange,
  AmountSpentInRangeQuery,
  Category,
  CategoryAmountSpent,
  Error
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private case object TestError extends ValidationError

  private class CategoryServiceTest
      extends CategoryService[IdMonad](
        stub[CategoryValidationAlgebra[IdMonad]],
        stub[CategoryRepository[IdMonad]],
        stub[TransactionRepository[IdMonad]]
      )
  private val mockCategoryService = stub[CategoryServiceTest]
  private val handler = new CategoryHandlerImpl(mockCategoryService)

  private val category = Category(5, Some(6), "name", "description", Vector.empty, Vector.empty)
  private val categoryModel =
    CategoryModel(Some(Id(5)), Some(Id(6)), Name("name"), Description("description"), Seq.empty, Seq.empty)
  private val categoryAmountSpent = CategoryAmountSpent(
    category = CategoryAmountSpent.Category(5, Some(6), "name", "description", Vector.empty, Vector.empty),
    budgetAmountSpent = Vector.empty
  )
  private val categoryAmountSpentModel = CategoryAmountSpentModel(
    category = categoryModel,
    spent = Seq.empty
  )

  "CategoryHandlerImpl" - {
    "createCategory" - {
      "should return CreateCategoryResponse.BadRequest on error in service" in {
        (mockCategoryService.create _).when(categoryModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createCategory(CreateCategoryResponse)(Some(category)) shouldEqual
          CreateCategoryResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateCategoryResponse.BadRequest on null body" in {
        handler.createCategory(CreateCategoryResponse)(None) shouldEqual
          CreateCategoryResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateCategoryResponse.Ok with category on successful create in service" in {
        (mockCategoryService.create _).when(categoryModel.copy(id = None)).returns(EitherT.rightT(categoryModel))

        handler.createCategory(CreateCategoryResponse)(Some(category)) shouldEqual
          CreateCategoryResponse.Ok(category)
      }
    }
    "updateCategory" - {
      val id = 1
      "should return UpdateCategoryResponse.BadRequest on error in service" in {
        (mockCategoryService.update _).when(categoryModel.copy(id = Some(Id(id)))).returns(EitherT.leftT(TestError))

        handler.updateCategory(UpdateCategoryResponse)(id, Some(category)) shouldEqual
          UpdateCategoryResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdateCategoryResponse.BadRequest on null body" in {
        handler.updateCategory(UpdateCategoryResponse)(id, None) shouldEqual
          UpdateCategoryResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateCategoryResponse.Ok with category on successful update in service" in {
        (mockCategoryService.update _)
          .when(categoryModel.copy(id = Some(Id(id))))
          .returns(EitherT.rightT(categoryModel))

        handler.updateCategory(UpdateCategoryResponse)(id, Some(category)) shouldEqual
          UpdateCategoryResponse.Ok(category)
      }
    }
    "deleteCategory" - {
      val id = 1
      "should return DeleteCategoryResponse.BadRequest on error in service" in {
        (mockCategoryService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteCategory(DeleteCategoryResponse)(id) shouldEqual
          DeleteCategoryResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeleteCategoryResponse.Ok with on successful delete in service" in {
        (mockCategoryService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteCategory(DeleteCategoryResponse)(id) shouldEqual DeleteCategoryResponse.Ok
      }
    }
    "getAllCategories" - {
      "should return GetAllCategoriesResponse.Ok with categories from service" in {
        (mockCategoryService.getAll _).when().returns(Seq(categoryModel, categoryModel, categoryModel))

        handler.getAllCategories(GetAllCategoriesResponse)() shouldEqual
          GetAllCategoriesResponse.Ok(Vector(category, category, category))
      }
    }
    "getAmountSpentInRange" - {
      val query = AmountSpentInRangeQuery(AmountSpentInRangeQuery.Range(LocalDate.now, LocalDate.now))
      val dateRangeModel = DateRangeModel(
        OffsetDateTime.of(query.range.start, LocalTime.MIN, ZoneOffset.UTC),
        OffsetDateTime.of(query.range.end, LocalTime.MIN, ZoneOffset.UTC)
      )
      val dateRange = AmountSpentInRange.Range(query.range.start, query.range.end)
      "should return GetAmountSpentInRangeResponse.Ok with amounts from service" in {
        (mockCategoryService.getAmountSpentInRange _)
          .when(dateRangeModel)
          .returns(Seq(categoryAmountSpentModel, categoryAmountSpentModel, categoryAmountSpentModel))

        handler.getAmountSpentInRange(GetAmountSpentInRangeResponse)(query) shouldEqual
          GetAmountSpentInRangeResponse.Ok(
            AmountSpentInRange(
              categories = Vector(categoryAmountSpent, categoryAmountSpent, categoryAmountSpent),
              range = dateRange
            )
          )
      }
    }
  }
}
