package com.finance.business.operations

import com.finance.business.model.types.DateRange
import com.finance.business.operations.CategoryOps._
import com.github.nscala_time.time.Imports._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryOpsSpec extends AnyFreeSpec with Matchers {

  "CategoryOps" - {
    "EffectiveTimeOperations" - {
      val range = DateRange(DateTime.lastWeek, DateTime.nextWeek)
      "within" - {
        "returns false when range not within any range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now)
          ) shouldBe false
        }
        "returns true when range equal to another range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now),
            range
          ) shouldBe true
        }
        "returns true when range within to another range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now),
            DateRange(DateTime.lastMonth, DateTime.nextMonth)
          ) shouldBe true
        }
      }
      "contains" - {
        "returns false when time is after range" in {
          range contains range.end.plusDays(1) shouldBe false
        }
        "returns false when time is before range" in {
          range contains range.start.minusDays(1) shouldBe false
        }
        "returns true when time is equal to range upper bound" in {
          range contains range.end shouldBe true
        }
        "returns true when time is equal to range lower bound" in {
          range contains range.start shouldBe true
        }
        "returns true when time is within range" in {
          range contains DateTime.now shouldBe true
        }
      }
    }
  }
}