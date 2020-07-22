package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.payback.{Payback => PaybackModel, PaybackBalance => PaybackBalanceModel}
import com.finance.business.model.transaction.{PaybackAmount => PaybackAmountModel}
import com.finance.business.model.types.{Description, Id, Name, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.PaybackMapping._
import com.finance.service.endpoints.definitions.{Payback, PaybackAmount, PaybackBalance}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PaybackMappingSpec extends AnyFreeSpec with Matchers {
  private val localDate = LocalDate.now
  private val offsetDateTime = OffsetDateTime.of(localDate, LocalTime.MIN, ZoneOffset.UTC)
  "PaybackMapping" - {
    "should contain implicits that" - {
      "map payback request to payback model" in {
        Payback(
          id = 5,
          name = "name",
          description = "description",
          date = localDate
        ).mapTo[PaybackModel] shouldEqual PaybackModel(
          id = Some(Id(5)),
          name = Name("name"),
          description = Description("description"),
          date = offsetDateTime
        )
      }
      "map payback model to payback response" in {
        PaybackModel(
          id = Some(Id(5)),
          name = Name("name"),
          description = Description("description"),
          date = offsetDateTime
        ).mapTo[Payback] shouldEqual Payback(
          id = 5,
          name = "name",
          description = "description",
          date = localDate
        )
      }
      "map payback balance model to payback balance response" in {
        PaybackBalanceModel(
          payback = PaybackModel(
            id = Some(Id(5)),
            name = Name("name"),
            description = Description("description"),
            date = offsetDateTime
          ),
          amounts = Seq(
            PaybackAmountModel(Id(5), Id(6), Usd(50.0), Description("desc0"), offsetDateTime),
            PaybackAmountModel(Id(6), Id(7), Usd(40.0), Description("desc1"), offsetDateTime.plusDays(1)),
            PaybackAmountModel(Id(7), Id(8), Usd(60.0), Description("desc2"), offsetDateTime.plusDays(2))
          )
        ).mapTo[PaybackBalance] shouldEqual PaybackBalance(
          payback = PaybackBalance.Payback(
            id = 5,
            name = "name",
            description = "description",
            date = localDate
          ),
          paybackAmounts = Vector(
            PaybackAmount(6, 50.0, "desc0", localDate),
            PaybackAmount(7, 40.0, "desc1", localDate.plusDays(1)),
            PaybackAmount(8, 60.0, "desc2", localDate.plusDays(2))
          )
        )
      }
    }
  }
}
