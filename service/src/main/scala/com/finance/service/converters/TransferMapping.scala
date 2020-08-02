package com.finance.service.converters

import java.time.{LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.transfer.{Transfer => TransferModel}
import com.finance.business.model.types.{Id, Usd}
import com.finance.service.endpoints.definitions.Transfer

object TransferMapping {
  implicit val transferRequestMapping: Mapping[Transfer, TransferModel] = (a: Transfer) =>
    TransferModel(
      id = Some(Id(a.id)),
      from = Id(a.from),
      fromDate = OffsetDateTime.of(a.fromDate, LocalTime.MIN, ZoneOffset.UTC),
      to = Id(a.to),
      toDate = OffsetDateTime.of(a.toDate, LocalTime.MIN, ZoneOffset.UTC),
      amount = Usd(a.amount)
    )

  implicit val transferResponseMapping: Mapping[TransferModel, Transfer] = (a: TransferModel) =>
    Transfer(
      id = a.id.map(_.value).getOrElse(-1),
      from = a.from.value,
      fromDate = a.fromDate.toLocalDate,
      to = a.to.value,
      toDate = a.toDate.toLocalDate,
      amount = a.amount.value
    )
}
