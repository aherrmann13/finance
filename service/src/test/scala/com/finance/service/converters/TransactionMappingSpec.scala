package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.transaction.{
  CategoryAmount => CategoryAmountModel,
  PaybackAmount => PaybackAmountModel,
  Transaction => TransactionModel
}
import com.finance.business.model.types.{Description, Id, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.TransactionMapping._
import com.finance.service.endpoints.definitions.{Amount, Transaction}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransactionMappingSpec extends AnyFreeSpec with Matchers {
  private val localDate = LocalDate.now
  private val offsetDateTime = OffsetDateTime.of(localDate, LocalTime.MIN, ZoneOffset.UTC)
  "TransactionMapping" - {
    "should contain implicits that" - {
      "map transaction request to transaction model" in {
        Transaction(
          id = 6,
          amounts = Vector(
            Amount(9, Amount.Type.members.Payback, 10, 50.0, "amtDesc0", localDate),
            Amount(10, Amount.Type.members.Category, 11, 60.0, "amtDesc1", localDate)
          ),
          description = "desc",
          transactionDate = localDate,
          accountId = 7
        ).mapTo[TransactionModel] shouldEqual TransactionModel(
          id = Some(Id(6)),
          description = Description("desc"),
          transactionDate = offsetDateTime,
          accountId = Id(7),
          amounts = Seq(
            PaybackAmountModel(Id(9), Id(10), Usd(50.0), Description("amtDesc0"), offsetDateTime),
            CategoryAmountModel(Id(10), Id(11), Usd(60.0), Description("amtDesc1"), offsetDateTime)
          )
        )
      }
      "map transaction model to transaction response" in {
        TransactionModel(
          id = Some(Id(6)),
          description = Description("desc"),
          transactionDate = offsetDateTime,
          accountId = Id(7),
          amounts = Seq(
            PaybackAmountModel(Id(9), Id(10), Usd(50.0), Description("amtDesc0"), offsetDateTime),
            CategoryAmountModel(Id(10), Id(11), Usd(60.0), Description("amtDesc1"), offsetDateTime)
          )
        ).mapTo[Transaction] shouldEqual Transaction(
          id = 6,
          amounts = Vector(
            Amount(9, Amount.Type.members.Payback, 10, 50.0, "amtDesc0", localDate),
            Amount(10, Amount.Type.members.Category, 11, 60.0, "amtDesc1", localDate)
          ),
          description = "desc",
          transactionDate = localDate,
          accountId = 7
        )
      }
    }
  }
}
