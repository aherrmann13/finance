package com.finance.service.handlers

import java.time.OffsetDateTime

import cats.{Id => IdMonad}
import com.finance.business.model.reporting.{AccountBalance => AccountBalanceModel, AccountValue => AccountValueModel}
import com.finance.business.model.types.{Id, Usd, DateRange => DateRangeModel}
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.{AccountRepository, AssetRepository, TransactionRepository}
import com.finance.business.services.ReportingService
import com.finance.business.services.query.{AccountValueQuery => AccountValueQueryModel}
import com.finance.service.endpoints.definitions.{
  AccountBalance,
  AccountBalanceQuery,
  AccountValue,
  AccountValueQuery,
  Error
}
import com.finance.service.endpoints.reporting.{GetAccountBalanceResponse, GetAccountValueResponse, GetNetWorthResponse}
import com.finance.service.time.TimeProvider
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ReportingHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {

  private val date = OffsetDateTime.now
  private implicit def idTimeProviderInstance: TimeProvider[IdMonad] =
    new TimeProvider[IdMonad] {
      override def now: IdMonad[OffsetDateTime] = date
    }

  private class ReportingServiceTest
      extends ReportingService[IdMonad](
        stub[AccountRepository[IdMonad]],
        stub[TransactionRepository[IdMonad]],
        stub[AssetRepository[IdMonad]],
        stub[StockPriceRetriever[IdMonad]]
      )

  private val mockReportingService = stub[ReportingServiceTest]
  private val handler = new ReportingHandlerImpl[IdMonad](mockReportingService)

  private val accountValue = AccountValue(
    dateRange = AccountValue.DateRange(date.toLocalDate, date.toLocalDate),
    accountId = 5,
    value = 50.0
  )
  private val accountValueModel = AccountValueModel(
    dateRange = DateRangeModel(date, date),
    accountId = Id(5),
    value = Usd(50.0)
  )

  "ReportingHandlerImpl" - {
    "getAccountValue" - {
      val query = AccountValueQuery(Vector.empty, Vector.empty, Some(true), Some(true))
      val queryModel = AccountValueQueryModel(Seq.empty, Set.empty, Some(true), Some(true))
      "should return GetAccountValueResponse.BadRequest on null body" in {
        handler.getAccountValue(GetAccountValueResponse)(None) shouldEqual
          GetAccountValueResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreatePaybackResponse.Ok with values from service" in {
        (mockReportingService.getAccountValue _)
          .when(queryModel, date)
          .returns(Seq(accountValueModel, accountValueModel))

        handler.getAccountValue(GetAccountValueResponse)(Some(query)) shouldEqual
          GetAccountValueResponse.Ok(Vector(accountValue, accountValue))
      }
    }
    "getNetWorth" - {
      "should return GetNetWorthResponse.Ok with value from service" in {
        (mockReportingService.getNetWorth _).when(date).returns(Usd(1000.0))

        handler.getNetWorth(GetNetWorthResponse)() shouldEqual GetNetWorthResponse.Ok(1000.0)
      }
    }
    "getAccountBalance" - {
      "should return CreatePaybackResponse.Ok with values from service called with empty acct ids seq" in {
        (mockReportingService.getAccountBalance _)
          .when(Set.empty[Id], date)
          .returns(Seq(AccountBalanceModel(Id(1), Usd(100)), AccountBalanceModel(Id(2), Usd(200))))

        handler.getAccountBalance(GetAccountBalanceResponse)() shouldEqual
          GetAccountBalanceResponse.Ok(Vector(AccountBalance(1, 100), AccountBalance(2, 200)))
      }
      "should return CreatePaybackResponse.Ok with values from service" in {
        val query = AccountBalanceQuery(Vector(1, 2))
        (mockReportingService.getAccountBalance _)
          .when(Set(Id(1), Id(2)), date)
          .returns(Seq(AccountBalanceModel(Id(1), Usd(100)), AccountBalanceModel(Id(2), Usd(200))))

        handler.getAccountBalance(GetAccountBalanceResponse)(Some(query)) shouldEqual
          GetAccountBalanceResponse.Ok(Vector(AccountBalance(1, 100), AccountBalance(2, 200)))
      }
    }
  }
}
