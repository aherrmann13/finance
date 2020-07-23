package com.finance.service.handlers

import cats.{Id => IdMonad}
import com.finance.business.model.asset.{Stock => StockModel}
import com.finance.business.model.record.{AssetRecord, Record => RecordModel}
import com.finance.business.model.types.Id
import com.finance.business.repository.{AssetRepository, TransactionRepository, TransferRepository}
import com.finance.business.services.RecordService
import com.finance.business.services.query.{RecordQuery => RecordQueryModel}
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.{Asset, Record, RecordQuery, Stock}
import com.finance.service.endpoints.record.GetRecordResponse
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RecordHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private class RecordServiceTest
      extends RecordService[IdMonad](
        stub[AssetRepository[IdMonad]],
        stub[TransferRepository[IdMonad]],
        stub[TransactionRepository[IdMonad]]
      )

  private val mockRecordService = stub[RecordServiceTest]
  private val handler = new RecordHandlerImpl(mockRecordService)

  private val record = Record(
    asset = Some(Asset(Some(Stock(5, 6, "ticker", Vector.empty)))),
    transfer = None,
    transaction = None
  )
  private val recordModel: RecordModel = AssetRecord(StockModel(Some(Id(5)), Id(6), "ticker", Seq.empty))

  "RecordHandlerImpl" - {
    "getRecord" - {
      val query = RecordQuery(None, None, Some(Vector(1, 2, 3)))
      val queryModel = RecordQueryModel(None, None, Set(Id(1), Id(2), Id(3)))
      "should return GetRecordResponse.Ok with records from service" in {
        (mockRecordService.getRecords _).when(queryModel).returns(Seq(recordModel, recordModel))

        handler.getRecord(GetRecordResponse)(Some(query)) shouldEqual
          GetRecordResponse.Ok(Vector(record, record))
      }
      "should return GetRecordResponse.Ok with records from service when no query provided" in {
        (mockRecordService.getRecords _).when(RecordQueryModel()).returns(Seq(recordModel, recordModel))

        handler.getRecord(GetRecordResponse)(None) shouldEqual
          GetRecordResponse.Ok(Vector(record, record))
      }
    }
  }

}
