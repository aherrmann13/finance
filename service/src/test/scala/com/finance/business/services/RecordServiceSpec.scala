package com.finance.business.services

import java.time.OffsetDateTime

import cats.{Id => IdMonad}
import com.finance.business.model.asset.{Buy, Stock}
import com.finance.business.model.record.{AssetRecord, TransactionRecord, TransferRecord}
import com.finance.business.model.transaction.Transaction
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.types.{Description, Id, Usd}
import com.finance.business.repository.query.{StockQuery, TransactionQuery, TransferQuery}
import com.finance.business.repository.{AssetRepository, TransactionRepository, TransferRepository}
import com.finance.business.services.query.RecordQuery
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RecordServiceSpec extends AnyFreeSpec with Matchers with MockFactory {

  private val mockAssetRepository = stub[AssetRepository[IdMonad]]
  private val mockTransferRepository = stub[TransferRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]

  private val service = new RecordService[IdMonad](
    mockAssetRepository,
    mockTransferRepository,
    mockTransactionRepository
  )

  private val fakeStock = Stock(Some(Id(2)), Id(3), "ticker", Seq(Buy(OffsetDateTime.now, 12, Usd(60), Usd(65))))
  private val fakeTransfer = Transfer(Some(Id(4)), Id(5), OffsetDateTime.now, Id(6), OffsetDateTime.now, Usd(50))
  private val fakeTransaction = Transaction(Some(Id(2)), Description("desc"), OffsetDateTime.now, Id(3), Seq.empty)

  private val query = RecordQuery(
    from = Some(OffsetDateTime.parse("2020-01-01T00:00:00Z")),
    to = Some(OffsetDateTime.parse("2020-02-01T00:00:00Z")),
    accountIds = Set(Id(1), Id(2))
  )

  "RecordService" - {
    "getRecords" - {
      "should return all assets in date range with account ids" in {
        val s0 = fakeStock.copy(ticker = "a")
        val s1 = fakeStock.copy(ticker = "b")
        val s2 = fakeStock.copy(ticker = "b")

        (mockAssetRepository
          .getStocks(_: StockQuery))
          .when(StockQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(s0, s1, s2))
        (mockTransferRepository.get(_: TransferQuery)).when(*).returns(Seq.empty)
        (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)

        service.getRecords(query) shouldEqual Seq(AssetRecord(s0), AssetRecord(s1), AssetRecord(s2))
      }
      "should return all transfers in date range with account ids" in {
        val f0 = fakeTransfer.copy(id = Some(Id(6)))
        val f1 = fakeTransfer.copy(id = Some(Id(7)))
        val f2 = fakeTransfer.copy(id = Some(Id(8)))

        (mockAssetRepository.getStocks(_: StockQuery)).when(*).returns(Seq.empty)
        (mockTransferRepository
          .get(_: TransferQuery))
          .when(TransferQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(f0, f1, f2))
        (mockTransactionRepository.get(_: TransactionQuery)).when(*).returns(Seq.empty)

        service.getRecords(query) shouldEqual Seq(TransferRecord(f0), TransferRecord(f1), TransferRecord(f2))
      }
      "should return all transactions in date range with account ids" in {
        val a0 = fakeTransaction.copy(id = Some(Id(6)))
        val a1 = fakeTransaction.copy(id = Some(Id(7)))
        val a2 = fakeTransaction.copy(id = Some(Id(8)))

        (mockAssetRepository.getStocks(_: StockQuery)).when(*).returns(Seq.empty)
        (mockTransferRepository.get(_: TransferQuery)).when(*).returns(Seq.empty)
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(a0, a1, a2))

        service.getRecords(query) shouldEqual Seq(TransactionRecord(a0), TransactionRecord(a1), TransactionRecord(a2))
      }
      "should return all record types concatenated" in {
        val s0 = fakeStock.copy(ticker = "a")
        val s1 = fakeStock.copy(ticker = "b")
        val s2 = fakeStock.copy(ticker = "b")
        val f0 = fakeTransfer.copy(id = Some(Id(6)))
        val f1 = fakeTransfer.copy(id = Some(Id(7)))
        val f2 = fakeTransfer.copy(id = Some(Id(8)))
        val a0 = fakeTransaction.copy(id = Some(Id(6)))
        val a1 = fakeTransaction.copy(id = Some(Id(7)))
        val a2 = fakeTransaction.copy(id = Some(Id(8)))

        (mockAssetRepository
          .getStocks(_: StockQuery))
          .when(StockQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(s0, s1, s2))
        (mockTransferRepository
          .get(_: TransferQuery))
          .when(TransferQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(f0, f1, f2))
        (mockTransactionRepository
          .get(_: TransactionQuery))
          .when(TransactionQuery(to = query.to, from = query.from, accountIds = query.accountIds))
          .returns(Seq(a0, a1, a2))

        service.getRecords(query) shouldEqual Seq(
          AssetRecord(s0),
          AssetRecord(s1),
          AssetRecord(s2),
          TransferRecord(f0),
          TransferRecord(f1),
          TransferRecord(f2),
          TransactionRecord(a0),
          TransactionRecord(a1),
          TransactionRecord(a2)
        )
      }
    }
  }
}
