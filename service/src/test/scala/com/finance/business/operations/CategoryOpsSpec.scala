package com.finance.business.operations

import com.finance.business.model.category.{Always, Collection, Range, Single}
import com.finance.business.operations.CategoryOps._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CategoryOpsSpec extends AnyFreeSpec with Matchers {

  "EffectiveTimeOperations" - {
    "within" - {
      "when anotherTime is Always " - {
        "and time is Always" - {
          "should return true" in {
            Always within Always shouldBe true
          }
        }
        "and time is Collection" - {
          "should return true" in {
            Collection(Seq(1, 2, 3)) within Always shouldBe true
          }
        }
        "and time is Range" - {
          "should return true" in {
            Range(1, 2) within Always shouldBe true
          }
        }
        "and time is Single" - {
          "should return true" in {
            Single(1) within Always shouldBe true
          }
        }
      }
      "when anotherTime is Collection " - {
        "and time is Always" - {
          "should return false" in {
            Always within Collection(Seq(1, 2, 3)) shouldBe false
          }
        }
        "and time is Collection" - {
          "with all elements in anothertime should return true" in {
            Collection(Seq(2, 3)) within Collection(Seq(1, 2, 3, 4)) shouldBe true
          }
          "with any element not in anothertime should return false" in {
            Collection(Seq(1, 2, 3)) within Collection(Seq(2, 3, 4)) shouldBe false
          }
        }
        "and time is Range" - {
          "inside collection should return true" in {
            Range(1, 4) within Collection(Seq(1, 2, 3, 4)) shouldBe true
          }
          "with elements not in collection should return false" in {
            Range(1, 3) within Collection(Seq(1, 3)) shouldBe false
          }
        }
        "and time is Single" - {
          "in collection should return true" in {
            Single(1) within Collection(Seq(1, 3)) shouldBe true
          }
          "outside collection should return false" in {
            Single(5) within Collection(Seq(1, 3)) shouldBe false
          }
        }
      }
      "when anotherTime is Range" - {
        "and time is Always" - {
          "should return false" in {
            Always within Range(4, 8) shouldBe false
          }
        }
        "and time is Collection" - {
          "with all elements within Range should return true" in {
            Collection(Seq(4, 6, 8)) within Range(4, 8) shouldBe true
          }
          "with any element outside Range should return false" in {
            Collection(Seq(4, 6, 9)) within Range(4, 8) shouldBe false
          }
        }
        "and time is Range" - {
          "that is identical should return true" in {
            Range(4, 8) within Range(4, 8) shouldBe true
          }
          "inside another time should return true" in {
            Range(5, 7) within Range(4, 8) shouldBe true
          }
          "outside another time should return false" in {
            Range(3, 9) within Range(4, 8) shouldBe false
          }
        }
        "and time is Single" - {
          "inside Range should return true" in {
            Single(5) within Range(4, 8) shouldBe true
          }
          "on edge should return true" in {
            Single(5) within Range(4, 8) shouldBe true
          }
          "outside of Range should return false" in {
            Single(2) within Range(4, 8) shouldBe false
          }
        }
      }
      "when anotherTime is Single" - {
        "and time is Always" - {
          "should return false" in {
            Always within Single(5) shouldBe false
          }
        }
        "and time is Collection" - {
          "with single element in Single should return true" in {
            Collection(Seq(5)) within Single(5) shouldBe true
          }
          "with single element not in Single should return false" in {
            Collection(Seq(6)) within Single(5) shouldBe false
          }
          "with multiple elements in Single should return true" in {
            Collection(Seq(5, 6)) within Single(5) shouldBe false
          }
        }
        "and time is Range" - {
          "of one period equal to single should return true" in {
            Range(5, 5) within Single(5) shouldBe true
          }
          "of one period not equal to single should return false" in {
            Range(5, 6) within Single(5) shouldBe false
          }
          "should return false" in {
            Range(5, 7) within Single(5) shouldBe false
          }
        }
        "and time is Single" - {
          "with different id should return false" in {
            Single(9) within Single(5) shouldBe false
          }
          "with same id should return true" in {
            Single(5) within Single(5) shouldBe true
          }
        }
      }
    }
  }
}