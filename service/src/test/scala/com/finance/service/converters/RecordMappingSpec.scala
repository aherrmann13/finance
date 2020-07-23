package com.finance.service.converters

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.asset.{Stock => StockModel}
import com.finance.business.model.record.{AssetRecord, Record => RecordModel}
import com.finance.business.model.types.Id
import com.finance.business.services.query.{RecordQuery => RecordQueryModel}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.RecordMapping._
import com.finance.service.endpoints.definitions.{Asset, Record, RecordQuery, Stock}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RecordMappingSpec extends AnyFreeSpec with Matchers {

  "RecordMapping" - {
    "should contain implicits that" - {
      "map record query request to record query model" in {
        RecordQuery(
          from = Some(LocalDate.of(2020, 1, 1)),
          to = Some(LocalDate.of(2020, 2, 1)),
          accounts = Some(Vector(1, 2, 3))
        ).mapTo[RecordQueryModel] shouldEqual RecordQueryModel(
          from = Some(OffsetDateTime.of(LocalDate.of(2020, 1, 1), LocalTime.MIN, ZoneOffset.UTC)),
          to = Some(OffsetDateTime.of(LocalDate.of(2020, 2, 1), LocalTime.MIN, ZoneOffset.UTC)),
          accountIds = Set(Id(1), Id(2), Id(3))
        )
      }
      "map record query request with no values to record query model" in {
        RecordQuery(from = None, to = None, accounts = None).mapTo[RecordQueryModel] shouldEqual
          RecordQueryModel(from = None, to = None, accountIds = Set.empty)
      }
      "map record model to record response" - {
        "when asset model" in {
          val record: RecordModel = AssetRecord(StockModel(Some(Id(5)), Id(6), "ticker", Seq.empty))

          record.mapTo[Record] shouldEqual
            Record(
              asset = Some(Asset(Some(Stock(5, 6, "ticker", Vector.empty)))),
              transfer = None,
              transaction = None
            )
        }
        // TODO: transfer and transaction here when done
      }
    }
  }
}
