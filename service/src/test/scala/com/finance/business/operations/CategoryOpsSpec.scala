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
        "should return false when range not within any range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now)
          ) shouldBe false
        }
        "should return true when range equal to another range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now),
            range
          ) shouldBe true
        }
        "should return true when range within to another range in list" in {
          range within Seq(
            DateRange(DateTime.lastYear, DateTime.lastMonth),
            DateRange(DateTime.lastMonth, DateTime.now),
            DateRange(DateTime.lastMonth, DateTime.nextMonth)
          ) shouldBe true
        }
      }
      "contains" - {
        "should return false when time is after range" in {
          range contains range.end.plusDays(1) shouldBe false
        }
        "should return false when time is before range" in {
          range contains range.start.minusDays(1) shouldBe false
        }
        "should return true when time is equal to range upper bound" in {
          range contains range.end shouldBe true
        }
        "should return true when time is equal to range lower bound" in {
          range contains range.start shouldBe true
        }
        "should return true when time is within range" in {
          range contains DateTime.now shouldBe true
        }
      }
      "overlaps" - {
        "should return false when no overlap" in {
          val r0 = DateRange(DateTime.lastYear, DateTime.lastMonth)
          val r1 = DateRange(DateTime.nextMonth, DateTime.nextYear)

          r0 overlaps r1 shouldBe false
          r1 overlaps r0 shouldBe false
        }
        "should return true when matching edge" in {
          val r0 = DateRange(DateTime.lastYear, DateTime.lastMonth)
          val r1 = DateRange(r0.end, DateTime.nextYear)

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
        "should return true when there is partial overlap" in {
          val r0 = DateRange(DateTime.lastYear, DateTime.now)
          val r1 = DateRange(DateTime.lastMonth, DateTime.nextYear)

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
        "should return true when there is total overlap" in {
          val r0 = DateRange(DateTime.lastYear, DateTime.nextYear)
          val r1 = DateRange(DateTime.lastMonth, DateTime.nextMonth)

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
      }
    }
  }
}