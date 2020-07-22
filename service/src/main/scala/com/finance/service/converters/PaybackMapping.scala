package com.finance.service.converters

import java.time.{LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.payback.{Payback => PaybackModel, PaybackBalance => PaybackBalanceModel}
import com.finance.business.model.transaction.{PaybackAmount => PaybackAmountModel}
import com.finance.business.model.types.{Description, Id, Name}
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{Payback, PaybackAmount, PaybackBalance}

object PaybackMapping {
  implicit val paybackRequestMapping: Mapping[Payback, PaybackModel] = (a: Payback) =>
    PaybackModel(
      id = Some(Id(a.id)),
      name = Name(a.name),
      description = Description(a.description),
      date = OffsetDateTime.of(a.date, LocalTime.MIN, ZoneOffset.UTC)
    )

  implicit val paybackResponseMapping: Mapping[PaybackModel, Payback] = (a: PaybackModel) =>
    Payback(
      id = a.id.map(_.value).getOrElse(-1),
      name = a.name.value,
      description = a.description.value,
      date = a.date.toLocalDate
    )

  private implicit val nestedPaybackResponseMapping: Mapping[PaybackModel, PaybackBalance.Payback] =
    (a: PaybackModel) =>
      PaybackBalance.Payback(
        id = a.id.map(_.value).getOrElse(-1),
        name = a.name.value,
        description = a.description.value,
        date = a.date.toLocalDate
      )

  private implicit val paybackAmountResponseMapping: Mapping[PaybackAmountModel, PaybackAmount] =
    (a: PaybackAmountModel) =>
      PaybackAmount(
        sourceId = a.sourceId.value,
        amount = a.amount.value,
        description = a.description.value,
        reportingDate = a.reportingDate.toLocalDate
      )

  implicit val paybackBalanceResponseMapping: Mapping[PaybackBalanceModel, PaybackBalance] = (a: PaybackBalanceModel) =>
    PaybackBalance(
      payback = a.payback.mapTo[PaybackBalance.Payback],
      paybackAmounts = a.amounts.map(_.mapTo[PaybackAmount]).toVector
    )
}
