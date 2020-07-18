package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.category.{
  Budget => BudgetModel,
  BudgetAmountSpent => BudgetAmountSpentModel,
  Category => CategoryModel,
  CategoryAmountSpent => CategoryAmountSpentModel
}
import com.finance.business.model.types.{Description, Id, Name, Usd, DateRange => DateRangeModel}
import com.finance.service.converters.CategoryMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent, DateRange}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryMappingSpec extends AnyFreeSpec with Matchers {

  private val localDate0 = LocalDate.now
  private val localDate1 = LocalDate.now.plusDays(1)
  private val localDate2 = LocalDate.now.plusDays(2)
  private val localDate3 = LocalDate.now.plusDays(3)

  private val offsetDateTime0 = OffsetDateTime.of(localDate0, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime1 = OffsetDateTime.of(localDate1, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime2 = OffsetDateTime.of(localDate2, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime3 = OffsetDateTime.of(localDate3, LocalTime.MIN, ZoneOffset.UTC)

  "CategoryMapping" - {
    "should contain implicits that" - {
      "map category request to category model" in {
        Category(
          5,
          Some(6),
          "name",
          "description",
          Vector(DateRange(localDate0, localDate1), DateRange(localDate2, localDate3)),
          Vector(
            Budget(Vector(DateRange(localDate0, localDate1)), 200.0),
            Budget(Vector(DateRange(localDate2, localDate3)), 200.0)
          )
        ).mapTo[CategoryModel] shouldEqual CategoryModel(
          Some(Id(5)),
          Some(Id(6)),
          Name("name"),
          Description("description"),
          Seq(DateRangeModel(offsetDateTime0, offsetDateTime1), DateRangeModel(offsetDateTime2, offsetDateTime3)),
          Seq(
            BudgetModel(Seq(DateRangeModel(offsetDateTime0, offsetDateTime1)), Usd(200.0)),
            BudgetModel(Seq(DateRangeModel(offsetDateTime2, offsetDateTime3)), Usd(200.0))
          )
        )
      }

      "map category model to category response" in {
        CategoryModel(
          Some(Id(5)),
          Some(Id(6)),
          Name("name"),
          Description("description"),
          Seq(DateRangeModel(offsetDateTime0, offsetDateTime1), DateRangeModel(offsetDateTime2, offsetDateTime3)),
          Seq(
            BudgetModel(Seq(DateRangeModel(offsetDateTime0, offsetDateTime1)), Usd(200.0)),
            BudgetModel(Seq(DateRangeModel(offsetDateTime2, offsetDateTime3)), Usd(200.0))
          )
        ).mapTo[Category] shouldEqual Category(
          5,
          Some(6),
          "name",
          "description",
          Vector(DateRange(localDate0, localDate1), DateRange(localDate2, localDate3)),
          Vector(
            Budget(Vector(DateRange(localDate0, localDate1)), 200.0),
            Budget(Vector(DateRange(localDate2, localDate3)), 200.0)
          )
        )
      }

      "map category amount spent model to category amount spent response" in {
        CategoryAmountSpentModel(
          category = CategoryModel(
            Some(Id(5)),
            Some(Id(6)),
            Name("name"),
            Description("description"),
            Seq(DateRangeModel(offsetDateTime0, offsetDateTime1), DateRangeModel(offsetDateTime2, offsetDateTime3)),
            Seq(
              BudgetModel(Seq(DateRangeModel(offsetDateTime0, offsetDateTime1)), Usd(200.0)),
              BudgetModel(Seq(DateRangeModel(offsetDateTime2, offsetDateTime3)), Usd(200.0))
            )
          ),
          spent = Seq(
            BudgetAmountSpentModel(Seq(DateRangeModel(offsetDateTime0, offsetDateTime1)), Usd(50), Usd(50), Usd(200)),
            BudgetAmountSpentModel(Seq(DateRangeModel(offsetDateTime2, offsetDateTime3)), Usd(60), Usd(60), Usd(200))
          )
        ).mapTo[CategoryAmountSpent] shouldEqual CategoryAmountSpent(
          category = CategoryAmountSpent.Category(
            5,
            Some(6),
            "name",
            "description",
            Vector(DateRange(localDate0, localDate1), DateRange(localDate2, localDate3)),
            Vector(
              Budget(Vector(DateRange(localDate0, localDate1)), 200.0),
              Budget(Vector(DateRange(localDate2, localDate3)), 200.0)
            )
          ),
          budgetAmountSpent = Vector(
            BudgetAmountSpent(Vector(DateRange(localDate0, localDate1)), 50.0, 50.0, 200.0),
            BudgetAmountSpent(Vector(DateRange(localDate2, localDate3)), 60.0, 60.0, 200.0)
          )
        )
      }
    }
  }
}
