package com.finance.business.operations

import com.finance.business.model.category.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent}
import com.finance.business.model.transaction.CategoryAmount
import com.finance.business.model.types.{DateRange, Usd}
import com.finance.business.operations.CategoryOps._

object TransactionOps {

  // TODO: cleanup
  implicit class CategoryAmountSeqOperations(amounts: Seq[CategoryAmount]) {
    def categoryValues(range: DateRange, categories: Seq[Category]): Seq[CategoryAmountSpent] = {
      val amountByCat = amounts.groupBy(_.categoryId)

      categories.map { category =>
        CategoryAmountSpent(
          category,
          category.budget.map { budget =>
            category.id
              .map { id =>
                budgetValue(budget, range, amountByCat.getOrElse(id, Seq.empty))
              }
              .getOrElse(BudgetAmountSpent(budget.effectiveTime, Usd(0), Usd(0), budget.amount))
          }
        )
      }
    }

    private def budgetValue(budget: Budget, dateRange: DateRange, amounts: Seq[CategoryAmount]): BudgetAmountSpent = {
      val inOutSplit = amounts.filter { amount =>
        budget.effectiveTime.exists(_ contains amount.reportingDate)
      } partition {
        dateRange contains _.reportingDate
      }

      BudgetAmountSpent(
        budget.effectiveTime,
        Usd(inOutSplit._1.map(_.amount.value).sum),
        Usd(inOutSplit._2.map(_.amount.value).sum),
        budget.amount
      )
    }
  }

}
