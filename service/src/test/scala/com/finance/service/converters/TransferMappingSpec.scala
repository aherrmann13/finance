package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.transfer.{Transfer => TransferModel}
import com.finance.business.model.types.{Id, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.TransferMapping._
import com.finance.service.endpoints.definitions.Transfer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransferMappingSpec extends AnyFreeSpec with Matchers {
  private val localDate0 = LocalDate.now
  private val localDate1 = LocalDate.now
  private val offsetDateTime0 = OffsetDateTime.of(localDate0, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime1 = OffsetDateTime.of(localDate1, LocalTime.MIN, ZoneOffset.UTC)

  "TransferMapping" - {
    "should contain implicits that" - {
      "map transfer request to transfer model" in {
        Transfer(
          id = 6,
          from = 7,
          fromDate = localDate0,
          to = 8,
          toDate = localDate1,
          amount = 150
        ).mapTo[TransferModel] shouldEqual TransferModel(
          id = Some(Id(6)),
          from = Id(7),
          fromDate = offsetDateTime0,
          to = Id(8),
          toDate = offsetDateTime1,
          amount = Usd(150)
        )
      }
      "map transfer model to transfer response" in {
        TransferModel(
          id = Some(Id(6)),
          from = Id(7),
          fromDate = offsetDateTime0,
          to = Id(8),
          toDate = offsetDateTime1,
          amount = Usd(150)
        ).mapTo[Transfer] shouldEqual
          Transfer(
            id = 6,
            from = 7,
            fromDate = localDate0,
            to = 8,
            toDate = localDate1,
            amount = 150
          )
      }
    }
  }
}
