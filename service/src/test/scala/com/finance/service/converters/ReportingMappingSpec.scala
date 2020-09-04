package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.reporting.{AccountBalance => AccountBalanceModel, AccountValue => AccountValueModel}
import com.finance.business.model.types.{Id, Usd, DateRange => DateRangeModel}
import com.finance.business.services.query.{AccountValueQuery => AccountValueQueryModel}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.ReportingMapping._
import com.finance.service.endpoints.definitions.{
  AccountBalance,
  AccountBalanceQuery,
  AccountValue,
  AccountValueQuery,
  DateRange
}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ReportingMappingSpec extends AnyFreeSpec with Matchers {

  private val localDate0 = LocalDate.now
  private val localDate1 = LocalDate.now.plusDays(1)
  private val localDate2 = LocalDate.now.plusDays(2)
  private val localDate3 = LocalDate.now.plusDays(3)

  private val offsetDateTime0 = OffsetDateTime.of(localDate0, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime1 = OffsetDateTime.of(localDate1, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime2 = OffsetDateTime.of(localDate2, LocalTime.MIN, ZoneOffset.UTC)
  private val offsetDateTime3 = OffsetDateTime.of(localDate3, LocalTime.MIN, ZoneOffset.UTC)

  "ReportingMapping" - {
    "should contain implicits that" - {
      "maps account value query request to account value model" in {
        AccountValueQuery(
          dateRanges = Vector(DateRange(localDate0, localDate1), DateRange(localDate2, localDate3)),
          accountIds = Vector(1, 2, 3),
          useReportingDate = Some(true),
          countAssetGrowthInPurchaseMonth = Some(true)
        ).mapTo[AccountValueQueryModel] shouldEqual AccountValueQueryModel(
          dateRanges = Seq(
            DateRangeModel(offsetDateTime0, offsetDateTime1),
            DateRangeModel(offsetDateTime2, offsetDateTime3)
          ),
          accountIds = Set(Id(1), Id(2), Id(3)),
          useReportingDate = Some(true),
          countAssetGrowthInPurchaseMonth = Some(true)
        )
      }
      "maps account value model to account value response" in {
        AccountValueModel(
          dateRange = DateRangeModel(offsetDateTime0, offsetDateTime1),
          accountId = Id(5),
          value = Usd(50.0)
        ).mapTo[AccountValue] shouldEqual AccountValue(
          dateRange = AccountValue.DateRange(localDate0, localDate1),
          accountId = 5,
          value = 50.0
        )
      }
      "maps account balance query request to list of account ids" in {
        AccountBalanceQuery(
          accountIds = Vector(1, 2, 3, 4)
        ).mapTo[Set[Id]] shouldEqual Set(Id(1), Id(2), Id(3), Id(4))
      }
      "maps account balance model to account balance response" in {
        AccountBalanceModel(
          accountId = Id(6),
          balance = Usd(50)
        ).mapTo[AccountBalance] shouldEqual AccountBalance(
          accountId = 6,
          balance = 50
        )
      }
    }
  }
}
