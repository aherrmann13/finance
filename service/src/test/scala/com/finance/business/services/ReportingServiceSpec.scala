package com.finance.business.services

import java.time.OffsetDateTime

import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.asset._
import com.finance.business.model.reporting.{AccountBalance, AccountValue}
import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types._
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.query.{StockQuery, TransactionQuery}
import com.finance.business.repository.{AccountRepository, AssetRepository, TransactionRepository}
import com.finance.business.services.query.AccountValueQuery
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ReportingServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val January = DateRange(
    OffsetDateTime.parse("2020-01-01T00:00:00Z"),
    OffsetDateTime.parse("2020-01-31T00:00:00Z")
  )
  private val February = DateRange(
    OffsetDateTime.parse("2020-02-01T00:00:00Z"),
    OffsetDateTime.parse("2020-02-28T00:00:00Z")
  )
  private val mockAccountRepository = stub[AccountRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]
  private val mockAssetRepository = stub[AssetRepository[IdMonad]]
  private val mockStockPriceRetriever = stub[StockPriceRetriever[IdMonad]]

  private val service = new ReportingService[IdMonad](
    mockAccountRepository,
    mockTransactionRepository,
    mockAssetRepository,
    mockStockPriceRetriever
  )

  private val fakeAccount = Account(Some(Id(6)), Name("Name"), Description("Description"), Bank, Usd(100))
  private val fakeAmount = CategoryAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
  private val fakeTransaction =
    Transaction(Some(Id(2)), Description("desc"), OffsetDateTime.now, Id(3), Seq(fakeAmount))
  private val fakeStock = Stock(Some(Id(2)), Id(3), "ticker", Seq(Buy(OffsetDateTime.now, 12, Usd(60), Usd(65))))
  private val date = OffsetDateTime.now

  // TODO: compare `Seq` better (order independent but exact same elements)
  "ReportingService" - {
    "getNetWorth" - {
      "should return value of all account initial amounts" in {
        val a0 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(50))
        val a1 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(60))
        val a2 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(70))

        (mockAccountRepository.getAll _).when().returns(Seq(a0, a1, a2))
        (mockTransactionRepository.get(_: TransactionQuery)).when(TransactionQuery(to = Some(date))).returns(Seq.empty)
        (mockAssetRepository.getStocks _).when(StockQuery(to = Some(date))).returns(Seq.empty)

        service.getNetWorth(date) shouldEqual Usd(180)
      }
      "should return value of all transactions" in {
        val t0 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(-30)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(-60)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          )
        )
        val t1 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(400)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(300))
          )
        )
        val t2 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), amount = Usd(50)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), amount = Usd(-25)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          )
        )

        (mockAccountRepository.getAll _).when().returns(Seq.empty)
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(to = Some(date)))
          .returns(Seq(t0, t1, t2))
        (mockAssetRepository.getStocks _).when(StockQuery(to = Some(date))).returns(Seq.empty)

        service.getNetWorth(date) shouldEqual Usd(635)
      }
      "should return value of all stocks" in {
        val s0 = fakeStock.copy(
          ticker = "a",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
            Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
          )
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55)),
            LifoSell(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 2, Usd(10), Usd(20))
          )
        )

        (mockAccountRepository.getAll _).when().returns(Seq.empty)
        (mockTransactionRepository.get(_: TransactionQuery)).when(TransactionQuery(to = Some(date))).returns(Seq.empty)
        (mockAssetRepository.getStocks _).when(StockQuery(to = Some(date))).returns(Seq(s0, s1))
        (mockStockPriceRetriever.call _)
          .when(s0.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(10), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s1.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(8), OffsetDateTime.now))

        service.getNetWorth(date) shouldEqual Usd(244)
      }
      "should return value of accounts stocks and transactions" in {
        val a0 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(50))
        val a1 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(60))
        val a2 = fakeAccount.copy(id = Some(Id(5)), initialAmount = Usd(70))

        val t0 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(-30)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(-60)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          )
        )
        val t1 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(400)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(300))
          )
        )
        val t2 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), amount = Usd(50)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), amount = Usd(-25)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          )
        )

        val s0 = fakeStock.copy(
          ticker = "a",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
            Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
          )
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55)),
            LifoSell(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 2, Usd(10), Usd(20))
          )
        )

        (mockAccountRepository.getAll _).when().returns(Seq(a0, a1, a2))
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(to = Some(date)))
          .returns(Seq(t0, t1, t2))
        (mockAssetRepository.getStocks _).when(StockQuery(to = Some(date))).returns(Seq(s0, s1))
        (mockStockPriceRetriever.call _)
          .when(s0.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(10), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s1.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(8), OffsetDateTime.now))

        service.getNetWorth(date) shouldEqual Usd(1059)
      }
    }
    "getAccountBalance" - {
      "should return balance of transactions by account" in {
        val t0 = fakeTransaction.copy(
          amounts = Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(-30)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(-60)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          ),
          accountId = Id(fakeTransaction.accountId.value + 1)
        )
        val t1 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(400)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(300))
          )
        )
        val t2 = fakeTransaction.copy(amounts =
          Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), amount = Usd(50)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), amount = Usd(-25)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          )
        )

        val accountIds = Set(t0.accountId, t1.accountId)
        val date = OffsetDateTime.parse("2020-01-01T00:00:00Z")

        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(accountIds = accountIds))
          .returns(Seq(t0, t1, t2))
        (mockAssetRepository.getStocks _).when(StockQuery(accountIds = accountIds)).returns(Seq.empty)

        val result: Seq[AccountBalance] = service.getAccountBalance(accountIds, date)
        result should have size 2
        result should contain(AccountBalance(t0.accountId, Usd(-70)))
        result should contain(AccountBalance(t1.accountId, Usd(745)))
      }
      "should return balance of stock by account" in {
        val s0 = fakeStock.copy(
          ticker = "a",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
            Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
          ),
          accountId = Id(fakeTransaction.accountId.value + 1)
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55)),
            LifoSell(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 2, Usd(10), Usd(20))
          )
        )
        val s2 = fakeStock.copy(
          ticker = "c",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(55))
          )
        )

        val accountIds = Set(s0.accountId, s1.accountId)
        val date = OffsetDateTime.parse("2020-01-01T00:00:00Z")

        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(accountIds = accountIds))
          .returns(Seq.empty)
        (mockAssetRepository.getStocks _).when(StockQuery(accountIds = accountIds)).returns(Seq(s0, s1, s2))

        (mockStockPriceRetriever.call _)
          .when(s0.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(10), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s1.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(8), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s2.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(30), OffsetDateTime.now))

        val result: Seq[AccountBalance] = service.getAccountBalance(accountIds, date)
        result should have size 2
        result should contain(AccountBalance(s0.accountId, Usd(200)))
        result should contain(AccountBalance(s1.accountId, Usd(344)))
      }
      "should return balance of transactions and stocks by account" in {
        val accountId0 = fakeTransaction.accountId
        val accountId1 = Id(fakeTransaction.accountId.value + 1)
        val t0 = fakeTransaction.copy(
          amounts = Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(-30)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(-60)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          ),
          accountId = accountId0
        )
        val t1 = fakeTransaction.copy(
          amounts = Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amount = Usd(400)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amount = Usd(300))
          ),
          accountId = accountId1
        )
        val t2 = fakeTransaction.copy(
          amounts = Seq(
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), amount = Usd(50)),
            fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), amount = Usd(-25)),
            PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
          ),
          accountId = accountId1
        )
        val s0 = fakeStock.copy(
          ticker = "a",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
            Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
          ),
          accountId = accountId1
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55)),
            LifoSell(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 2, Usd(10), Usd(20))
          ),
          accountId = accountId0
        )
        val s2 = fakeStock.copy(
          ticker = "c",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(55))
          ),
          accountId = accountId0
        )

        val accountIds = Set(accountId0, accountId1)
        val date = OffsetDateTime.parse("2020-01-01T00:00:00Z")

        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(accountIds = accountIds))
          .returns(Seq(t0, t1, t2))
        (mockAssetRepository.getStocks _).when(StockQuery(accountIds = accountIds)).returns(Seq(s0, s1, s2))

        (mockStockPriceRetriever.call _)
          .when(s0.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(10), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s1.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(8), OffsetDateTime.now))
        (mockStockPriceRetriever.call _)
          .when(s2.ticker, date)
          .returns(StockPriceAsOf(Usd(6), Usd(30), OffsetDateTime.now))

        val result: Seq[AccountBalance] = service.getAccountBalance(accountIds, date)
        result should have size 2
        result should contain(AccountBalance(s0.accountId, Usd(945)))
        result should contain(AccountBalance(s1.accountId, Usd(274)))
      }
    }
    "getAccountValue" - {
      "when useReportingDate is None should return transaction amount value as if it was Some(false)" in {
        val t0 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"))
        val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, None)

        val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)

        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
          .returns(Seq(t0))
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
          .returns(Seq.empty)
        (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

        val result: Seq[AccountValue] = service.getAccountValue(q, date)
        result should have size 2
        result should contain(AccountValue(January, t0.accountId, Usd(20)))
        result should contain(AccountValue(February, t0.accountId, Usd(0)))
      }
      "when useReportingDate is false" - {
        "should return amount value on transaction date" in {
          val t0 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"))
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)

          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(20)))
          result should contain(AccountValue(February, t0.accountId, Usd(0)))
        }
        "should not return transaction payback value" in {
          val t0 = fakeTransaction.copy(
            transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"),
            amounts = Seq(
              CategoryAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now),
              CategoryAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now),
              PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.now)
            )
          )
          val q = AccountValueQuery(Seq(January), Set(Id(3)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)

          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 1
          result should contain(AccountValue(January, t0.accountId, Usd(40)))
        }
        "should return amount value counted in date ranges transaction falls in" in {
          val t0 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"))
          val t1 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
          val t2 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"))
          val t3 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))

          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2, t3))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(40)))
          result should contain(AccountValue(February, t0.accountId, Usd(40)))
        }
        "should return amount value counted in all date ranges transaction falls in" in {
          val t0 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"))
          val t1 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
          val t2 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"))
          val t3 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))

          val q = AccountValueQuery(
            Seq(
              DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-02-14T00:00:00Z")),
              DateRange(OffsetDateTime.parse("2020-01-14T00:00:00Z"), OffsetDateTime.parse("2020-03-01T00:00:00Z"))
            ),
            Set(Id(3)),
            None,
            None
          )

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2, t3))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(q.dateRanges.head, t0.accountId, Usd(60)))
          result should contain(AccountValue(q.dateRanges(1), t0.accountId, Usd(60)))
        }
        "should return amount value counted in transaction accounts" in {
          val t0 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), accountId = Id(2))
          val t1 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), accountId = Id(3))
          val t2 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), accountId = Id(4))
          val t3 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), accountId = Id(4))

          val q = AccountValueQuery(Seq(January, February), Set(Id(2), Id(3), Id(4)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2, t3))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 6
          result should contain(AccountValue(January, Id(2), Usd(20)))
          result should contain(AccountValue(January, Id(3), Usd(20)))
          result should contain(AccountValue(January, Id(4), Usd(0)))
          result should contain(AccountValue(February, Id(2), Usd(0)))
          result should contain(AccountValue(February, Id(3), Usd(0)))
          result should contain(AccountValue(February, Id(4), Usd(40)))
        }
        "should return all requested accounts and date ranges regardless of transactions" in {
          val q = AccountValueQuery(Seq(January, February), Set(Id(2), Id(3), Id(4)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq.empty)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 6
          result should contain(AccountValue(January, Id(2), Usd(0)))
          result should contain(AccountValue(January, Id(3), Usd(0)))
          result should contain(AccountValue(January, Id(4), Usd(0)))
          result should contain(AccountValue(February, Id(2), Usd(0)))
          result should contain(AccountValue(February, Id(3), Usd(0)))
          result should contain(AccountValue(February, Id(4), Usd(0)))
        }
        "should return amount value of all amounts in transaction" in {
          val amt0 = CategoryAmount(Id(4), Id(5), Usd(50), Description("desc"), OffsetDateTime.now)
          val amt1 = CategoryAmount(Id(4), Id(5), Usd(80), Description("desc"), OffsetDateTime.now)
          val t0 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), amounts = Seq(amt0, amt1))
          val t1 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), amounts = Seq(amt0, amt1))
          val t2 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), amounts = Seq(amt0, amt1))
          val t3 = fakeTransaction
            .copy(transactionDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), amounts = Seq(amt0, amt1))

          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(false), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2, t3))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(260)))
          result should contain(AccountValue(February, t1.accountId, Usd(260)))
        }
      }
      "when useReportingDate is true" - {
        "should return amount value on amount date" in {
          val t0 = fakeTransaction.copy(
            amounts = Seq(fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")))
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(true), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)

          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(20)))
          result should contain(AccountValue(February, t0.accountId, Usd(0)))
        }
        "should not return transaction payback value" in {
          val t0 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              PaybackAmount(Id(4), Id(5), Usd(20), Description("desc"), OffsetDateTime.parse("2020-01-10T00:00:00Z"))
            )
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(true), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)

          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(20)))
          result should contain(AccountValue(February, t0.accountId, Usd(0)))
        }
        "should return amount value counted in date ranges amount falls in" in {
          val t0 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t1 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t2 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))
            )
          )

          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), Some(true), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(January, t0.accountId, Usd(80)))
          result should contain(AccountValue(February, t0.accountId, Usd(40)))
        }
        "should return amount value counted in all date ranges amount falls in" in {
          val t0 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t1 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t2 = fakeTransaction.copy(amounts =
            Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))
            )
          )

          val q = AccountValueQuery(
            Seq(
              DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-02-14T00:00:00Z")),
              DateRange(OffsetDateTime.parse("2020-01-14T00:00:00Z"), OffsetDateTime.parse("2020-03-01T00:00:00Z"))
            ),
            Set(Id(3)),
            Some(true),
            None
          )

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          result should contain(AccountValue(q.dateRanges.head, t0.accountId, Usd(100)))
          result should contain(AccountValue(q.dateRanges(1), t0.accountId, Usd(80)))
        }
        "should return amount value counted in transaction accounts" in {
          val t0 = fakeTransaction.copy(
            accountId = Id(3),
            amounts = Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t1 = fakeTransaction.copy(
            accountId = Id(4),
            amounts = Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"))
            )
          )
          val t2 = fakeTransaction.copy(
            accountId = Id(3),
            amounts = Seq(
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-10T00:00:00Z")),
              fakeAmount.copy(reportingDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"))
            )
          )

          val q = AccountValueQuery(Seq(January, February), Set(Id(3), Id(4)), Some(true), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq(t0, t1))
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq(t2))
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 4
          result should contain(AccountValue(January, Id(3), Usd(40)))
          result should contain(AccountValue(January, Id(4), Usd(40)))
          result should contain(AccountValue(February, Id(3), Usd(40)))
          result should contain(AccountValue(February, Id(4), Usd(0)))
        }
        "should return all requested accounts and date ranges regardless of transactions" in {
          val q = AccountValueQuery(Seq(January, February), Set(Id(2), Id(3), Id(4)), Some(true), None)

          val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
            .returns(Seq.empty)
          (mockTransactionRepository
            .get(_: TransactionQuery))
            .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
            .returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq.empty)

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 6
          result should contain(AccountValue(January, Id(2), Usd(0)))
          result should contain(AccountValue(January, Id(3), Usd(0)))
          result should contain(AccountValue(January, Id(4), Usd(0)))
          result should contain(AccountValue(February, Id(2), Usd(0)))
          result should contain(AccountValue(February, Id(3), Usd(0)))
          result should contain(AccountValue(February, Id(4), Usd(0)))
        }
      }
      "when countAssetGrowthInPurchaseMonth not set should return stock value as if it was Some(false)" in {
        val s0 = fakeStock.copy(
          ticker = "a",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
            Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
          )
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          actions = Seq(
            Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
          )
        )
        val s0Prices = Map(
          January.start -> StockPriceAsOf(Usd(20), Usd(25), January.start),
          January.end -> StockPriceAsOf(Usd(25), Usd(22), January.end),
          February.start -> StockPriceAsOf(Usd(22), Usd(23), February.start),
          February.end -> StockPriceAsOf(Usd(30), Usd(35), February.end)
        )
        val s1Prices = Map(
          January.start -> StockPriceAsOf(Usd(50), Usd(55), January.start),
          January.end -> StockPriceAsOf(Usd(60), Usd(65), January.end),
          February.start -> StockPriceAsOf(Usd(65), Usd(80), February.start),
          February.end -> StockPriceAsOf(Usd(80), Usd(100), February.end)
        )
        val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, None)
        (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
        (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1))
        s0Prices.foreach { priceWithDate =>
          (mockStockPriceRetriever
            .call(_: String, _: OffsetDateTime))
            .when(s0.ticker, priceWithDate._1)
            .returns(priceWithDate._2.pure[IdMonad])
        }
        s1Prices.foreach { priceWithDate =>
          (mockStockPriceRetriever
            .call(_: String, _: OffsetDateTime))
            .when(s1.ticker, priceWithDate._1)
            .returns(priceWithDate._2.pure[IdMonad])
        }

        val result: Seq[AccountValue] = service.getAccountValue(q, date)
        result should have size 2
        // math by hand as inlining the math is a little verbose
        // s0 = 20 stocks, value is 500, 440, 460, 700 at each date -> delta of -60 and 240
        // s1 = 5 stocks, value is 275, 325, 400, 500 at each date -> delta of 50 and 100
        result should contain(AccountValue(January, Id(3), Usd(-10)))
        result should contain(AccountValue(February, Id(3), Usd(340)))
      }
      "when countAssetGrowthInPurchaseMonth is false" - {
        "should return stock value as value at date range end minus value at date range beginning" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            actions = Seq(
              Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            actions = Seq(
              Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
            )
          )
          val s0Prices = Map(
            January.start -> StockPriceAsOf(Usd(20), Usd(25), January.start),
            January.end -> StockPriceAsOf(Usd(25), Usd(22), January.end),
            February.start -> StockPriceAsOf(Usd(22), Usd(23), February.start),
            February.end -> StockPriceAsOf(Usd(30), Usd(35), February.end)
          )
          val s1Prices = Map(
            January.start -> StockPriceAsOf(Usd(50), Usd(55), January.start),
            January.end -> StockPriceAsOf(Usd(60), Usd(65), January.end),
            February.start -> StockPriceAsOf(Usd(65), Usd(80), February.start),
            February.end -> StockPriceAsOf(Usd(80), Usd(100), February.end)
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, Some(false))
          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1))
          s0Prices.foreach { priceWithDate =>
            (mockStockPriceRetriever
              .call(_: String, _: OffsetDateTime))
              .when(s0.ticker, priceWithDate._1)
              .returns(priceWithDate._2.pure[IdMonad])
          }
          s1Prices.foreach { priceWithDate =>
            (mockStockPriceRetriever
              .call(_: String, _: OffsetDateTime))
              .when(s1.ticker, priceWithDate._1)
              .returns(priceWithDate._2.pure[IdMonad])
          }

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          // math by hand as inlining the math is a little verbose
          // s0 = 20 stocks, value is 500, 440, 460, 700 at each date -> delta of -60 and 240
          // s1 = 5 stocks, value is 275, 325, 400, 500 at each date -> delta of 50 and 100
          result should contain(AccountValue(January, Id(3), Usd(-10)))
          result should contain(AccountValue(February, Id(3), Usd(340)))
        }
        "should return stock value with values in date range counted as growth and past end ignored" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            actions = Seq(
              Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2019-02-01T00:00:00Z"), 10, Usd(11), Usd(115)),
              Buy(OffsetDateTime.parse("2020-01-15T00:00:00Z"), 10, Usd(15), Usd(155)),
              LifoSell(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 10, Usd(15), Usd(155)),
              Buy(OffsetDateTime.parse("2020-03-15T00:00:00Z"), 10, Usd(15), Usd(155))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            actions = Seq(
              Buy(OffsetDateTime.parse("2019-01-01T00:00:00Z"), 5, Usd(10), Usd(55)),
              CashDividend(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 1, Usd(10), Usd(10)),
              Buy(OffsetDateTime.parse("2020-03-01T00:00:00Z"), 100, Usd(15), Usd(1505))
            )
          )
          val s0Prices = Map(
            January.start -> StockPriceAsOf(Usd(20), Usd(21), January.start),
            January.end -> StockPriceAsOf(Usd(22), Usd(23), January.end),
            February.start -> StockPriceAsOf(Usd(24), Usd(25), February.start),
            February.end -> StockPriceAsOf(Usd(26), Usd(27), February.end)
          )
          val s1Prices = Map(
            January.start -> StockPriceAsOf(Usd(50), Usd(55), January.start),
            January.end -> StockPriceAsOf(Usd(60), Usd(65), January.end),
            February.start -> StockPriceAsOf(Usd(65), Usd(80), February.start),
            February.end -> StockPriceAsOf(Usd(80), Usd(100), February.end)
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, Some(false))
          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1))
          s0Prices.foreach { priceWithDate =>
            (mockStockPriceRetriever
              .call(_: String, _: OffsetDateTime))
              .when(s0.ticker, priceWithDate._1)
              .returns(priceWithDate._2.pure[IdMonad])
          }
          s1Prices.foreach { priceWithDate =>
            (mockStockPriceRetriever
              .call(_: String, _: OffsetDateTime))
              .when(s1.ticker, priceWithDate._1)
              .returns(priceWithDate._2.pure[IdMonad])
          }

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          // math by hand as inlining the math is a little verbose
          // s0 = 20 @ 21/share = 420, 30 @ 23/share = 690, 30 @ 25/share = 750, 20 @ 27/share = 540 (and 10 sold @ 155)
          // s1 = 5 @ 55/share = 275, 5 @ 65/share = 325, 5 @ 80/share = 400, 5 @ 100/share = 500 (and div of $10)
          // deltas: 270, -55 | 50, 110
          result should contain(AccountValue(January, Id(3), Usd(320)))
          result should contain(AccountValue(February, Id(3), Usd(55)))
        }
        "should return stock value counted in correct accounts" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            accountId = Id(3),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            accountId = Id(3),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
            )
          )
          val s2 = fakeStock.copy(
            ticker = "c",
            accountId = Id(4),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 20, Usd(10), Usd(205))
            )
          )
          val s3 = fakeStock.copy(
            ticker = "d",
            accountId = Id(5),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 1, Usd(100), Usd(105))
            )
          )
          val allPrices = Map(
            January.start -> StockPriceAsOf(Usd(10), Usd(10), January.start),
            January.end -> StockPriceAsOf(Usd(20), Usd(20), January.end),
            February.start -> StockPriceAsOf(Usd(30), Usd(30), February.start),
            February.end -> StockPriceAsOf(Usd(40), Usd(40), February.end)
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3), Id(4), Id(5)), None, Some(false))
          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1, s2, s3))
          allPrices.foreach { priceWithDate =>
            (mockStockPriceRetriever
              .call(_: String, _: OffsetDateTime))
              .when(*, priceWithDate._1)
              .returns(priceWithDate._2.pure[IdMonad])
          }

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 6
          // math by hand as inlining the math is a little verbose
          // s0 = 10 @ 10/share = 100, 10 @ 20/share = 200, 10 @ 30/share = 300, 10 @ 40/share = 400 in acct 3
          // s1 =  5 @ 10/share =  50,  5 @ 20/share = 100,  5 @ 30/share = 150,  5 @ 40/share = 200 in acct 3
          // s2 = 20 @ 30/share = 600, 20 @ 40/share = 800 in acct 4
          // s3 =  1 @ 30/share =  30,  1 @ 40/share =  40 in acct 5
          result should contain(AccountValue(January, Id(3), Usd(150)))
          result should contain(AccountValue(January, Id(4), Usd(0)))
          result should contain(AccountValue(January, Id(5), Usd(0)))
          result should contain(AccountValue(February, Id(3), Usd(150)))
          result should contain(AccountValue(February, Id(4), Usd(200)))
          result should contain(AccountValue(February, Id(5), Usd(10)))
        }
      }
      "when countAssetGrowthInPurchaseMonth is true" - {
        "should return stock value as growth in purchase month" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              LifoSell(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 2, Usd(20), Usd(40)),
              LifoSell(OffsetDateTime.parse("2020-05-01T00:00:00Z"), 2, Usd(30), Usd(60))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
            )
          )
          val s2 = fakeStock.copy(
            ticker = "c",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 10, Usd(15), Usd(155)),
              StockDividend(OffsetDateTime.parse("2020-06-01T00:00:00Z"), 1, Usd(20), Usd(20))
            )
          )
          val s0Price = StockPriceAsOf(Usd(20), Usd(30), OffsetDateTime.now)
          val s1Price = StockPriceAsOf(Usd(40), Usd(45), OffsetDateTime.now)
          val s2Price = StockPriceAsOf(Usd(20), Usd(22), OffsetDateTime.now)
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, Some(true))

          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1, s2))
          (mockStockPriceRetriever.call _).when(s0.ticker, date).returns(s0Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s1.ticker, date).returns(s1Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s2.ticker, date).returns(s2Price.pure[IdMonad])

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          // math by hand as inlining the math is a little verbose
          // s0 = 10 stocks, 4 sold for a total of 100, current price is 30/unit gives 280
          // s1 = 5 stocks, current price is 45/unit gives 225
          // s2 = 20 stocks, another as dividend, current price is 22/unit gives 462
          result should contain(AccountValue(January, Id(3), Usd(505)))
          result should contain(AccountValue(February, Id(3), Usd(462)))
        }
        "should return stock value as value of lifecycle with buy in date range" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              LifoSell(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 2, Usd(20), Usd(40)),
              LifoSell(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 2, Usd(30), Usd(60))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 10, Usd(15), Usd(155)),
              StockDividend(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 1, Usd(10), Usd(105))
            )
          )
          val q = AccountValueQuery(Seq(January, February), Set(Id(3)), None, Some(true))
          val s0Price = StockPriceAsOf(Usd(20), Usd(30), OffsetDateTime.now)
          val s1Price = StockPriceAsOf(Usd(40), Usd(45), OffsetDateTime.now)

          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1))
          (mockStockPriceRetriever.call _).when(s0.ticker, date).returns(s0Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s1.ticker, date).returns(s1Price.pure[IdMonad])
          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          // math by hand as inlining the math is a little verbose
          // s0 = 10 stocks, 4 sold for a total of 100, current price is 30/unit gives 280
          // s1 = 10 stocks in jan, 10 in feb with a split stock dividend of 1 stock gives 10.5 in each
          // current price is 45/unit gives 472.5 in jan and feb
          result should contain(AccountValue(January, Id(3), Usd(752.5)))
          result should contain(AccountValue(February, Id(3), Usd(472.5)))

        }
        "should return stock value counted in all date ranges the buy in lifecycle falls in" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              LifoSell(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 2, Usd(20), Usd(40)),
              LifoSell(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 2, Usd(30), Usd(60))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 10, Usd(15), Usd(155)),
              StockDividend(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 1, Usd(10), Usd(105))
            )
          )
          val q = AccountValueQuery(
            Seq(
              DateRange(OffsetDateTime.parse("2020-01-01T00:00:00Z"), OffsetDateTime.parse("2020-03-01T00:00:00Z")),
              DateRange(OffsetDateTime.parse("2020-01-15T00:00:00Z"), OffsetDateTime.parse("2020-03-01T00:00:00Z"))
            ),
            Set(Id(3)),
            None,
            Some(true)
          )
          val s0Price = StockPriceAsOf(Usd(20), Usd(30), OffsetDateTime.now)
          val s1Price = StockPriceAsOf(Usd(40), Usd(45), OffsetDateTime.now)

          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1))
          (mockStockPriceRetriever.call _).when(s0.ticker, date).returns(s0Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s1.ticker, date).returns(s1Price.pure[IdMonad])
          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 2
          // math by hand as inlining the math is a little verbose
          // s0 = 10 stocks, 4 sold for a total of 100, current price is 30/unit gives 280
          // s1 = 10 stocks in jan, 10 in feb with a split stock dividend of 1 stock gives 10.5 on each date
          // current price is 45/unit so each date 2020-01-01 and 2020-02-01 has a value of 472.5 each
          result should contain(AccountValue(q.dateRanges.head, Id(3), Usd(1225.0)))
          result should contain(AccountValue(q.dateRanges(1), Id(3), Usd(472.5)))
        }
        "should return stock value counted in correct accounts" in {
          val s0 = fakeStock.copy(
            ticker = "a",
            accountId = Id(4),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              LifoSell(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 2, Usd(20), Usd(40)),
              LifoSell(OffsetDateTime.parse("2020-05-01T00:00:00Z"), 2, Usd(30), Usd(60))
            )
          )
          val s1 = fakeStock.copy(
            ticker = "b",
            accountId = Id(4),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
            )
          )
          val s2 = fakeStock.copy(
            ticker = "c",
            accountId = Id(5),
            actions = Seq(
              Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105)),
              Buy(OffsetDateTime.parse("2020-02-15T00:00:00Z"), 10, Usd(15), Usd(155)),
              StockDividend(OffsetDateTime.parse("2020-06-01T00:00:00Z"), 1, Usd(20), Usd(20))
            )
          )
          val s0Price = StockPriceAsOf(Usd(20), Usd(30), OffsetDateTime.now)
          val s1Price = StockPriceAsOf(Usd(40), Usd(45), OffsetDateTime.now)
          val s2Price = StockPriceAsOf(Usd(20), Usd(22), OffsetDateTime.now)
          val q = AccountValueQuery(Seq(January, February), Set(Id(3), Id(4), Id(5)), None, Some(true))

          (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
          (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1, s2))
          (mockStockPriceRetriever.call _).when(s0.ticker, date).returns(s0Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s1.ticker, date).returns(s1Price.pure[IdMonad])
          (mockStockPriceRetriever.call _).when(s2.ticker, date).returns(s2Price.pure[IdMonad])

          val result: Seq[AccountValue] = service.getAccountValue(q, date)
          result should have size 6
          // math by hand as inlining the math is a little verbose
          // s0 = 10 stocks, 4 sold for a total of 100, current price is 30/unit gives 280 in account 4 in jan
          // s1 = 5 stocks, current price is 45/unit gives 225 in account 4 in jan
          // s2 = 20 stocks, 1 as dividend, current price is 22/unit gives 462 split between jan and feb in account 5
          result should contain(AccountValue(January, Id(3), Usd(0)))
          result should contain(AccountValue(January, Id(4), Usd(505)))
          result should contain(AccountValue(January, Id(5), Usd(231)))
          result should contain(AccountValue(February, Id(3), Usd(0)))
          result should contain(AccountValue(February, Id(4), Usd(0)))
          result should contain(AccountValue(February, Id(5), Usd(231)))
        }
      }
      "should return transactions and stock in same account in same date range added together" in {
        val t0 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-10T00:00:00Z"), accountId = Id(3))
        val t1 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-01-15T00:00:00Z"), accountId = Id(4))
        val t2 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-10T00:00:00Z"), accountId = Id(5))
        val t3 = fakeTransaction.copy(transactionDate = OffsetDateTime.parse("2020-02-15T00:00:00Z"), accountId = Id(5))

        val s0 = fakeStock.copy(
          ticker = "a",
          accountId = Id(3),
          actions = Seq(
            Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 10, Usd(10), Usd(105))
          )
        )
        val s1 = fakeStock.copy(
          ticker = "b",
          accountId = Id(3),
          actions = Seq(
            Buy(OffsetDateTime.parse("2020-01-01T00:00:00Z"), 5, Usd(10), Usd(55))
          )
        )
        val s2 = fakeStock.copy(
          ticker = "c",
          accountId = Id(4),
          actions = Seq(
            Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 20, Usd(10), Usd(205))
          )
        )
        val s3 = fakeStock.copy(
          ticker = "d",
          accountId = Id(5),
          actions = Seq(
            Buy(OffsetDateTime.parse("2020-02-01T00:00:00Z"), 1, Usd(100), Usd(105))
          )
        )
        val allPrices = Map(
          January.start -> StockPriceAsOf(Usd(10), Usd(10), January.start),
          January.end -> StockPriceAsOf(Usd(20), Usd(20), January.end),
          February.start -> StockPriceAsOf(Usd(30), Usd(30), February.start),
          February.end -> StockPriceAsOf(Usd(40), Usd(40), February.end)
        )
        val q = AccountValueQuery(Seq(January, February), Set(Id(3), Id(4), Id(5)), Some(false), Some(false))

        val expectedTransQuery = TransactionQuery(accountIds = q.accountIds, useReportingDate = q.useReportingDate)
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(expectedTransQuery.copy(from = Some(q.dateRanges.head.start), to = Some(q.dateRanges.head.end)))
          .returns(Seq(t0, t1))
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(expectedTransQuery.copy(from = Some(q.dateRanges(1).start), to = Some(q.dateRanges(1).end)))
          .returns(Seq(t2, t3))

        (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)
        (mockAssetRepository.getAllStocks _).when().returns(Seq(s0, s1, s2, s3))
        allPrices.foreach { priceWithDate =>
          (mockStockPriceRetriever
            .call(_: String, _: OffsetDateTime))
            .when(*, priceWithDate._1)
            .returns(priceWithDate._2.pure[IdMonad])
        }

        val result: Seq[AccountValue] = service.getAccountValue(q, date)
        result should have size 6
        // math for stocks the same as countAssetGrowthInPurchaseMonth false correct accounts test
        result should contain(AccountValue(January, Id(3), Usd(170)))
        result should contain(AccountValue(January, Id(4), Usd(20)))
        result should contain(AccountValue(January, Id(5), Usd(0)))
        result should contain(AccountValue(February, Id(3), Usd(150)))
        result should contain(AccountValue(February, Id(4), Usd(200)))
        result should contain(AccountValue(February, Id(5), Usd(50)))
      }
    }
  }
}
