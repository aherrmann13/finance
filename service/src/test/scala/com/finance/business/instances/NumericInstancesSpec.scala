package com.finance.business.instances

import cats.implicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.finance.business.instances.NumericInstances._
import com.finance.business.model.types.Usd

class NumericInstancesSpec extends AnyFreeSpec with Matchers {
  "NumericInstances" - {
    "invariant" - {
      "imap" - {
        val bigDecimalNumeric = implicitly[Numeric[BigDecimal]]
        val usdNumeric: Numeric[Usd] = bigDecimalNumeric.imap(Usd(_))(_.value)
        "plus" in {
          bigDecimalNumeric.plus(50.5, 60.5) shouldEqual usdNumeric.plus(Usd(50.5), Usd(60.5)).value
        }
        "minus" in {
          bigDecimalNumeric.minus(50.5, 60.5) shouldEqual usdNumeric.minus(Usd(50.5), Usd(60.5)).value
        }
        "times" in {
          bigDecimalNumeric.times(50.5, 60.5) shouldEqual usdNumeric.times(Usd(50.5), Usd(60.5)).value
        }
        "negate" in {
          bigDecimalNumeric.negate(50.5) shouldEqual usdNumeric.negate(Usd(50.5)).value
        }
        "fromInt" in {
          bigDecimalNumeric.fromInt(50) shouldEqual usdNumeric.fromInt(50).value
        }
        "parseString" in {
          bigDecimalNumeric.parseString("50") shouldEqual usdNumeric.parseString("50").map(_.value)
          bigDecimalNumeric.parseString("not a num") shouldEqual usdNumeric.parseString("not a num").map(_.value)
        }
        "toInt" in {
          bigDecimalNumeric.toInt(50.5) shouldEqual usdNumeric.toInt(Usd(50.5))
        }
        "toLong" in {
          bigDecimalNumeric.toLong(50.5) shouldEqual usdNumeric.toLong(Usd(50.5))
        }
        "toFloat" in {
          bigDecimalNumeric.toFloat(50.5) shouldEqual usdNumeric.toFloat(Usd(50.5))
        }
        "toDouble" in {
          bigDecimalNumeric.toDouble(50.5) shouldEqual usdNumeric.toDouble(Usd(50.5))
        }
        "compare" in {
          bigDecimalNumeric.compare(50.5, 60.5) shouldEqual usdNumeric.compare(Usd(50.5), Usd(60.5))
        }
      }
    }
  }
}
