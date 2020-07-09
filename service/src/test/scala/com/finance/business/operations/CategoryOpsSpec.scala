package com.finance.business.operations

import java.time.OffsetDateTime

import com.finance.business.model.types.DateRange
import com.finance.business.operations.CategoryOps._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryOpsSpec extends AnyFreeSpec with Matchers {

  "CategoryOps" - {
    "EffectiveTimeOperations" - {
      val range = DateRange(OffsetDateTime.now.minusWeeks(1), OffsetDateTime.now.plusWeeks(1))
      "within" - {
        "should return false when range not within any range in list" in {
          range within Seq(
            DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.minusMonths(1)),
            DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now)
          ) shouldBe false
        }
        "should return true when range equal to another range in list" in {
          range within Seq(
            DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.minusMonths(1)),
            DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now),
            range
          ) shouldBe true
        }
        "should return true when range within to another range in list" in {
          range within Seq(
            DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.minusMonths(1)),
            DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now),
            DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now.plusMonths(1))
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
          range contains OffsetDateTime.now shouldBe true
        }
      }
      "overlaps" - {
        "should return false when no overlap" in {
          val r0 = DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.minusMonths(1))
          val r1 = DateRange(OffsetDateTime.now.plusMonths(1), OffsetDateTime.now.plusYears(1))

          r0 overlaps r1 shouldBe false
          r1 overlaps r0 shouldBe false
        }
        "should return true when matching edge" in {
          val r0 = DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.minusMonths(1))
          val r1 = DateRange(r0.end, OffsetDateTime.now.plusYears(1))

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
        "should return true when there is partial overlap" in {
          val r0 = DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now)
          val r1 = DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now.plusYears(1))

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
        "should return true when there is total overlap" in {
          val r0 = DateRange(OffsetDateTime.now.minusYears(1), OffsetDateTime.now.plusYears(1))
          val r1 = DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now.plusMonths(1))

          r0 overlaps r1 shouldBe true
          r1 overlaps r0 shouldBe true
        }
      }
    }
  }
}