package com.finance.business.model.operations

import com.finance.business.model.category.{Always, EffectiveTime, Range, Single}
import com.finance.business.model.operations.Category._
import org.scalatest.{FreeSpec, Matchers}

class CategorySpec extends FreeSpec with Matchers {

  "EffectiveTimeOperations" - {
    "within" - {
      "when anotherTime is Always " - {
        "and time is Always should return true" in {
          Always within Always shouldBe true
        }
        "and time is Range should return true" in {
          Range(1, 2) within Always shouldBe true
        }
        "and time is Single should return true" in {
          Single(1) within Always shouldBe true
        }
      }
      "when anotherTime is Range" - {
        "and time is same Range should return true" in {
          Range(4, 8) within Range(4, 8) shouldBe true
        }
        "and time is inner Range should return true" in {
          Range(5, 7) within Range(4, 8) shouldBe true
        }
        "and time is outside Range should return false" in {
          Range(3, 9) within Range(4, 8) shouldBe false
        }
        "and time is inner Single should return true" in {
          Single(5) within Range(4, 8) shouldBe true
        }
        "and time is Single on edge should return true" in {
          Single(5) within Range(4, 8) shouldBe true
        }
        "and time is outside Single should return false" in {
          Single(2) within Range(4, 8) shouldBe false
        }
        "and time is Always should return false" in {
          Always within Range(4, 8) shouldBe false
        }
      }
      "when anotherTime is Single" - {
        "and time is Always should return false" in {
          Always within Single(5) shouldBe false
        }
        "and time is Range should return false" in {
          Range(5, 7) within Single(5) shouldBe false
        }
        "and time is Single with different id should return false" in {
          Single(9) within Single(5) shouldBe false
        }
        "and time is Single with same id should return true" in {
          Single(5) within Single(5) shouldBe true
        }
      }
    }
  }
}
