package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.services.RecordService
import com.finance.business.services.query.{RecordQuery => RecordQueryModel}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.RecordMapping._
import com.finance.service.endpoints.definitions.{Record, RecordQuery}
import com.finance.service.endpoints.record.{GetRecordResponse, RecordHandler}

class RecordHandlerImpl[F[_]: Monad](recordService: RecordService[F]) extends RecordHandler[F] {
  override def getRecord(respond: GetRecordResponse.type)(filter: Option[RecordQuery]): F[GetRecordResponse] =
    recordService.getRecords(filter.map(_.mapTo[RecordQueryModel]).getOrElse(RecordQueryModel())) map { records =>
      respond.Ok(records.map(_.mapTo[Record]).toVector)
    }
}
