package com.finance.service.converters

import java.time.{LocalDate, OffsetDateTime}

import com.finance.business.model.types.{DateRange => DateRangeModel}
import com.finance.service.converters.CommonMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{AmountSpentInRange, AmountSpentInRangeQuery, DateRange}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CommonMappingSpec extends AnyFreeSpec with Matchers {

  "CommonMapping" - {
    "should contain implicits that" - {
      "map date range request to date range model" in {
        DateRange(
          LocalDate.of(2020, 1, 1),
          LocalDate.of(2020, 2, 1)
        ).mapTo[DateRangeModel] shouldEqual DateRangeModel(
          OffsetDateTime.parse("2020-01-01T00:00:00.00Z"),
          OffsetDateTime.parse("2020-02-01T00:00:00.00Z")
        )
      }
      "map amount spent in range query range request to date range model" in {
        AmountSpentInRangeQuery
          .Range(
            LocalDate.of(2020, 1, 1),
            LocalDate.of(2020, 2, 1)
          )
          .mapTo[DateRangeModel] shouldEqual DateRangeModel(
          OffsetDateTime.parse("2020-01-01T00:00:00.00Z"),
          OffsetDateTime.parse("2020-02-01T00:00:00.00Z")
        )
      }
      "map date range model to date range response" in {
        DateRangeModel(
          OffsetDateTime.parse("2020-01-01T00:00:00.00Z"),
          OffsetDateTime.parse("2020-02-01T00:00:00.00Z")
        ).mapTo[DateRange] shouldEqual DateRange(
          LocalDate.of(2020, 1, 1),
          LocalDate.of(2020, 2, 1)
        )
      }

      "map date range model to amount spent in range range response" in {
        DateRangeModel(
          OffsetDateTime.parse("2020-01-01T00:00:00.00Z"),
          OffsetDateTime.parse("2020-02-01T00:00:00.00Z")
        ).mapTo[AmountSpentInRange.Range] shouldEqual AmountSpentInRange.Range(
          LocalDate.of(2020, 1, 1),
          LocalDate.of(2020, 2, 1)
        )
      }
    }
  }
}
