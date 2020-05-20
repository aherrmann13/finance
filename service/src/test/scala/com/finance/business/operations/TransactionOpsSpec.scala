package com.finance.business.operations

import com.finance.business.model.category.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent}
import com.finance.business.model.transaction.{CategoryAmount, Transaction}
import com.finance.business.operations.TransactionOps._
import com.finance.business.model.types.{DateRange, Description, Id, Name, Usd}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.github.nscala_time.time.Imports._


class TransactionOpsSpec extends AnyFreeSpec with Matchers {
  private val budget0 = Budget(
    Seq(
      DateRange(DateTime.parse("2019-01-01"), DateTime.parse("2019-01-31")),
      DateRange(DateTime.parse("2019-02-01"), DateTime.parse("2019-02-28"))
    ),
    Usd(50.00)
  )
  private val budget1 = Budget(
    Seq(
      DateRange(DateTime.parse("2019-03-01"), DateTime.parse("2019-03-31")),
      DateRange(DateTime.parse("2019-04-01"), DateTime.parse("2019-04-30"))
    ),
    Usd(50.00)
  )
  private val budget2 = Budget(
    Seq(
      DateRange(DateTime.parse("2019-05-01"), DateTime.parse("2019-05-31")),
      DateRange(DateTime.parse("2019-06-01"), DateTime.parse("2019-06-30"))
    ),
    Usd(50.00)
  )
  private val budget3 = Budget(
    Seq(
      DateRange(DateTime.parse("2019-07-01"), DateTime.parse("2019-07-31")),
      DateRange(DateTime.parse("2019-08-01"), DateTime.parse("2019-08-31"))
    ),
    Usd(50.00)
  )
  private val cat0 = Category(
    Some(Id(3)),
    Some(Id(4)),
    Name("name"),
    Description("description"),
    Seq(
      DateRange(DateTime.parse("2019-01-01"), DateTime.parse("2019-05-31")),
      DateRange(DateTime.parse("2019-06-01"), DateTime.parse("2019-12-31"))
    ),
    Seq(budget0, budget1)
  )
  private val cat1 = Category(
    Some(Id(5)),
    Some(Id(6)),
    Name("name"),
    Description("description"),
    Seq(
      DateRange(DateTime.parse("2018-01-01"), DateTime.parse("2018-12-31")),
      DateRange(DateTime.parse("2019-01-01"), DateTime.parse("2019-12-31"))
    ),
    Seq(budget2, budget3)
  )
  "TransactionOps" - {
    "categoryValues" - {
      "should copy all category and budget attributes to CategoryAmountSpent and BudgetAmountSpent" in {
        val dateRange = DateRange(DateTime.parse("2018-01-01"), DateTime.parse("2020-12-31"))
        Seq.empty[Transaction].categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(cat0, Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
            )),
            CategoryAmountSpent(cat1, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            ))
          )
      }
      "should put amounts inside date range in budget date range as 'in'" in {
        val dateRange = DateRange(DateTime.parse("2018-01-01"), DateTime.parse("2020-12-31"))
        val timeWithinBudget0FirstPeriod = budget0.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget0SecondPeriod = budget0.effectiveTime(1).start.plusDays(1)
        val timeWithinBudget1FirstPeriod = budget1.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget2FirstPeriod = budget2.effectiveTime.head.start.plusDays(1)
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget0SecondPeriod)
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt3 = CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), timeWithinBudget2FirstPeriod)
        val amt4 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget1FirstPeriod)

        val transaction0 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt0, amt1))
        val transaction1 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt2, amt3, amt4))

        Seq(transaction0, transaction1).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(cat0, Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(130), Usd(0), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(20), Usd(0), budget1.amount)
            )),
            CategoryAmountSpent(cat1, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(90), Usd(0), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            ))
          )
      }
      "should put amounts outside date range in budget date range as 'out'" in {
        val dateRange = DateRange(DateTime.parse("2020-01-01"), DateTime.parse("2020-12-31"))
        val timeWithinBudget0FirstPeriod = budget0.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget0SecondPeriod = budget0.effectiveTime(1).start.plusDays(1)
        val timeWithinBudget1FirstPeriod = budget1.effectiveTime.head.start.plusDays(1)
        val timeWithinBudget2FirstPeriod = budget2.effectiveTime.head.start.plusDays(1)
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget0SecondPeriod)
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), timeWithinBudget0FirstPeriod)
        val amt3 = CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), timeWithinBudget2FirstPeriod)
        val amt4 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), timeWithinBudget1FirstPeriod)

        val transaction0 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt0, amt1))
        val transaction1 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt2, amt3, amt4))

        Seq(transaction0, transaction1).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(cat0, Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(130), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(20), budget1.amount)
            )),
            CategoryAmountSpent(cat1, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(90), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            ))
          )
      }
      "should ignore amounts outside date range outside budget date range" in {
        val dateRange = DateRange(DateTime.parse("2020-01-01"), DateTime.parse("2020-12-31"))
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), DateTime.parse("2015-01-01"))
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), DateTime.parse("2015-01-01"))
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), DateTime.parse("2015-01-01"))
        val amt3 = CategoryAmount(cat1.id.get, Id(6), Usd(90), Description("desc"), DateTime.parse("2015-01-01"))
        val amt4 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), DateTime.parse("2015-01-01"))

        val transaction0 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt0, amt1))
        val transaction1 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt2, amt3, amt4))

        Seq(transaction0, transaction1).categoryValues(dateRange, Seq(cat0, cat1)) shouldEqual
          Seq(
            CategoryAmountSpent(cat0, Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
            )),
            CategoryAmountSpent(cat1, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            ))
          )
      }
      "should include amount in multiple budgets with containing date ranges" in {
        val budget4 = Budget(
          Seq(
            DateRange(DateTime.parse("2019-01-01"), DateTime.parse("2019-01-31")),
            DateRange(DateTime.parse("2019-02-01"), DateTime.parse("2019-02-28"))
          ),
          Usd(50.00)
        )
        val budget5 = Budget(
          Seq(
            DateRange(DateTime.parse("2019-02-01"), DateTime.parse("2019-02-28")),
            DateRange(DateTime.parse("2019-03-01"), DateTime.parse("2019-03-31"))
          ),
          Usd(50.00)
        )
        val cat2 = cat0.copy(budget = Seq(budget4, budget5))
        val dateRange = DateRange(DateTime.parse("2019-01-01"), DateTime.parse("2020-12-31"))
        val amt0 = CategoryAmount(cat0.id.get, Id(6), Usd(50), Description("desc"), DateTime.parse("2019-01-15"))
        val amt1 = CategoryAmount(cat0.id.get, Id(6), Usd(20), Description("desc"), DateTime.parse("2019-02-15"))
        val amt2 = CategoryAmount(cat0.id.get, Id(6), Usd(60), Description("desc"), DateTime.parse("2019-03-15"))

        val transaction0 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt0, amt1))
        val transaction1 = Transaction(Some(Id(5)), Description("desc"), DateTime.now, Id(6), Seq(amt2))

        Seq(transaction0, transaction1).categoryValues(dateRange, Seq(cat2)) shouldEqual
          Seq(
            CategoryAmountSpent(cat2, Seq(
              BudgetAmountSpent(budget4.effectiveTime, Usd(70), Usd(0), budget4.amount),
              BudgetAmountSpent(budget5.effectiveTime, Usd(80), Usd(0), budget5.amount)
            ))
          )
      }
      "should use empty BudgetAmountSpent if category id is None" in {
        val cat2 = cat1.copy(id = None)
        val dateRange = DateRange(DateTime.parse("2018-01-01"), DateTime.parse("2020-12-31"))
        Seq.empty[Transaction].categoryValues(dateRange, Seq(cat0, cat1, cat2)) shouldEqual
          Seq(
            CategoryAmountSpent(cat0, Seq(
              BudgetAmountSpent(budget0.effectiveTime, Usd(0), Usd(0), budget0.amount),
              BudgetAmountSpent(budget1.effectiveTime, Usd(0), Usd(0), budget1.amount)
            )),
            CategoryAmountSpent(cat1, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            )),
            CategoryAmountSpent(cat2, Seq(
              BudgetAmountSpent(budget2.effectiveTime, Usd(0), Usd(0), budget2.amount),
              BudgetAmountSpent(budget3.effectiveTime, Usd(0), Usd(0), budget3.amount)
            ))
          )
      }
    }
  }
}
