package com.finance.business.operations

import java.time.OffsetDateTime

import com.finance.business.model.category.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent}
import com.finance.business.model.transaction.CategoryAmount
import com.finance.business.operations.TransactionOps._
import com.finance.business.model.types.{DateRange, Description, Id, Name, Usd}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransactionOpsSpec extends AnyFreeSpec with Matchers {
  private val budget0 = Budget(
    Seq(
      DateRange(OffsetDateTime.parse("2019-01-01T00:00:00Z"), OffsetDateTime.parse("2019-01-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-02-01T00:00:00Z"), OffsetDateTime.parse("2019-02-28T00:00:00Z"))
    ),
    Usd(50.00)
  )
  private val budget1 = Budget(
    Seq(
      DateRange(OffsetDateTime.parse("2019-03-01T00:00:00Z"), OffsetDateTime.parse("2019-03-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-04-01T00:00:00Z"), OffsetDateTime.parse("2019-04-30T00:00:00Z"))
    ),
    Usd(50.00)
  )
  private val budget2 = Budget(
    Seq(
      DateRange(OffsetDateTime.parse("2019-05-01T00:00:00Z"), OffsetDateTime.parse("2019-05-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-06-01T00:00:00Z"), OffsetDateTime.parse("2019-06-30T00:00:00Z"))
    ),
    Usd(50.00)
  )
  private val budget3 = Budget(
    Seq(
      DateRange(OffsetDateTime.parse("2019-07-01T00:00:00Z"), OffsetDateTime.parse("2019-07-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-08-01T00:00:00Z"), OffsetDateTime.parse("2019-08-31T00:00:00Z"))
    ),
    Usd(50.00)
  )
  private val cat0 = Category(
    Some(Id(3)),
    Some(Id(4)),
    Name("name"),
    Description("description"),
    Seq(
      DateRange(OffsetDateTime.parse("2019-01-01T00:00:00Z"), OffsetDateTime.parse("2019-05-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-06-01T00:00:00Z"), OffsetDateTime.parse("2019-12-31T00:00:00Z"))
    ),
    Seq(budget0, budget1)
  )
  private val cat1 = Category(
    Some(Id(5)),
    Some(Id(6)),
    Name("name"),
    Description("description"),
    Seq(
      DateRange(OffsetDateTime.parse("2018-01-01T00:00:00Z"), OffsetDateTime.parse("2018-12-31T00:00:00Z")),
      DateRange(OffsetDateTime.parse("2019-01-01T00:00:00Z"), OffsetDateTime.parse("2019-12-31T00:00:00Z"))
    ),
    Seq(budget2, budget3)
  )
  "TransactionOps" - {
    "categoryValues" - {
      "should copy all category and budget attributes to CategoryAmountSpent and BudgetAmountSpent" in {
        val dateRange = DateRange(
          OffsetDateTime.parse("2018-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        Seq.empty[CategoryAmount].categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat0,
              Seq(
                BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
                BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
              )
            ),
            CategoryAmountSpent(
              cat1,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            )
          )
      }
      "should put amounts inside date range in budget date range as 'in'" in {
        val dateRange = DateRange(
          OffsetDateTime.parse("2018-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        val timeWithinBudget0FirstPeriod = budget0.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget0SecondPeriod = budget0.effectiveTime(1).start.plusDays(1)
        val timeWithinBudget1FirstPeriod = budget1.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget2FirstPeriod = budget2.effectiveTime.head.start.plusDays(1)
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget0SecondPeriod)
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt3 = CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), timeWithinBudget2FirstPeriod)
        val amt4 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget1FirstPeriod)

        Seq(amt0, amt1, amt2, amt3, amt4).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat0,
              Seq(
                BudgetAmountSpent(budget0.effectiveTime, Usd(130), Usd(0), budget0.amount),
                BudgetAmountSpent(budget1.effectiveTime, Usd(20), Usd(0), budget1.amount)
              )
            ),
            CategoryAmountSpent(
              cat1,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(90), Usd(0), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            )
          )
      }
      "should put amounts outside date range in budget date range as 'out'" in {
        val dateRange = DateRange(
          OffsetDateTime.parse("2020-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        val timeWithinBudget0FirstPeriod = budget0.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget0SecondPeriod = budget0.effectiveTime(1).start.plusDays(1)
        val timeWithinBudget1FirstPeriod = budget1.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget2FirstPeriod = budget2.effectiveTime.head.start.plusDays(1)
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget0SecondPeriod)
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt3 = CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), timeWithinBudget2FirstPeriod)
        val amt4 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget1FirstPeriod)

        Seq(amt0, amt1, amt2, amt3, amt4).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat0,
              Seq(
                BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(130), budget0.amount),
                BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(20), budget1.amount)
              )
            ),
            CategoryAmountSpent(
              cat1,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(90), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            )
          )
      }
      "should ignore amounts outside date range outside budget date range" in {
        val dateRange = DateRange(
          OffsetDateTime.parse("2020-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        val amt0 =
          CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), OffsetDateTime.parse("2015-01-01T00:00:00Z"))
        val amt1 =
          CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), OffsetDateTime.parse("2015-01-01T00:00:00Z"))
        val amt2 =
          CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), OffsetDateTime.parse("2015-01-01T00:00:00Z"))
        val amt3 =
          CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), OffsetDateTime.parse("2015-01-01T00:00:00Z"))
        val amt4 =
          CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), OffsetDateTime.parse("2015-01-01T00:00:00Z"))

        Seq(amt0, amt1, amt2, amt3, amt4).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat0,
              Seq(
                BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
                BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
              )
            ),
            CategoryAmountSpent(
              cat1,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            )
          )
      }
      "should include amount in multiple budgets with containing date ranges" in {
        val budget4 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2019-01-01T00:00:00Z"), OffsetDateTime.parse("2019-01-31T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2019-02-01T00:00:00Z"), OffsetDateTime.parse("2019-02-28T00:00:00Z"))
          ),
          Usd(50.00)
        )
        val budget5 = Budget(
          Seq(
            DateRange(OffsetDateTime.parse("2019-02-01T00:00:00Z"), OffsetDateTime.parse("2019-02-28T00:00:00Z")),
            DateRange(OffsetDateTime.parse("2019-03-01T00:00:00Z"), OffsetDateTime.parse("2019-03-31T00:00:00Z"))
          ),
          Usd(50.00)
        )
        val cat2 = cat0.copy(budget = Seq(budget4, budget5))
        val dateRange = DateRange(
          OffsetDateTime.parse("2019-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        val amt0 =
          CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), OffsetDateTime.parse("2019-01-15T00:00:00Z"))
        val amt1 =
          CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), OffsetDateTime.parse("2019-02-15T00:00:00Z"))
        val amt2 =
          CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), OffsetDateTime.parse("2019-03-15T00:00:00Z"))

        Seq(amt0, amt1, amt2).categoryValues(dateRange, Seq(cat2)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat2,
              Seq(
                BudgetAmountSpent(budget4.effectiveTime, Usd(70), Usd(0), budget4.amount),
                BudgetAmountSpent(budget5.effectiveTime, Usd(80), Usd(0), budget5.amount)
              )
            )
          )
      }
      "should use empty BudgetAmountSpent if category id is None" in {
        val cat2 = cat1.copy(id = None)
        val dateRange = DateRange(
          OffsetDateTime.parse("2018-01-01T00:00:00Z"),
          OffsetDateTime.parse("2020-12-31T00:00:00Z")
        )
        Seq.empty[CategoryAmount].categoryValues(dateRange, Seq(cat0, cat1, cat2)) shouldEqual
          Seq(
            CategoryAmountSpent(
              cat0,
              Seq(
                BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
                BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
              )
            ),
            CategoryAmountSpent(
              cat1,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            ),
            CategoryAmountSpent(
              cat2,
              Seq(
                BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
                BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
              )
            )
          )
      }
    }
  }
}
