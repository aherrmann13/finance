package com.finance.service.converters

import java.time.{LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.record.{AssetRecord, TransactionRecord, TransferRecord, Record => RecordModel}
import com.finance.business.model.types.Id
import com.finance.business.services.query.{RecordQuery => RecordQueryModel}
import com.finance.service.converters.AssetMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.TransactionMapping._
import com.finance.service.endpoints.definitions.{Asset, Record, RecordQuery, Transaction}

object RecordMapping {

  implicit val recordQueryRequestMapping: Mapping[RecordQuery, RecordQueryModel] = (a: RecordQuery) =>
    RecordQueryModel(
      from = a.from.map(OffsetDateTime.of(_, LocalTime.MIN, ZoneOffset.UTC)),
      to = a.to.map(OffsetDateTime.of(_, LocalTime.MIN, ZoneOffset.UTC)),
      accountIds = a.accounts.getOrElse(Vector.empty).map(Id(_)).toSet
    )

  implicit val recordResponseMapping: Mapping[RecordModel, Record] = {
    case AssetRecord(value) => Record(transaction = None, transfer = None, asset = Some(value.mapTo[Asset]))
    case TransactionRecord(value) => Record(transaction = Some(value.mapTo[Transaction]), transfer = None, asset = None)
    case TransferRecord(_) => Record(transaction = None, transfer = None, asset = None)
  }
}
