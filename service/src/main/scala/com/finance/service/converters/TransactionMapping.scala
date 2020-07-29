package com.finance.service.converters

import java.time.{LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.transaction.{
  Amount => AmountModel,
  CategoryAmount => CategoryAmountModel,
  PaybackAmount => PaybackAmountModel,
  Transaction => TransactionModel
}
import com.finance.business.model.types.{Description, Id, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{Amount, Transaction}

object TransactionMapping {
  private implicit val transactionAmountRequestMapping: Mapping[Amount, AmountModel] = (a: Amount) =>
    a.`type` match {
      case Amount.Type.members.Category =>
        CategoryAmountModel(
          categoryId = Id(a.relationId),
          sourceId = Id(a.sourceId),
          amount = Usd(a.amount),
          description = Description(a.description),
          reportingDate = OffsetDateTime.of(a.reportingDate, LocalTime.MIN, ZoneOffset.UTC)
        )
      case Amount.Type.members.Payback =>
        PaybackAmountModel(
          paybackId = Id(a.relationId),
          sourceId = Id(a.sourceId),
          amount = Usd(a.amount),
          description = Description(a.description),
          reportingDate = OffsetDateTime.of(a.reportingDate, LocalTime.MIN, ZoneOffset.UTC)
        )
    }

  private implicit val transactionAmountResponseMapping: Mapping[AmountModel, Amount] = {
    case category: CategoryAmountModel =>
      Amount(
        relationId = category.categoryId.value,
        `type` = Amount.Type.members.Category,
        sourceId = category.sourceId.value,
        amount = category.amount.value,
        description = category.description.value,
        reportingDate = category.reportingDate.toLocalDate
      )
    case payback: PaybackAmountModel =>
      Amount(
        relationId = payback.paybackId.value,
        `type` = Amount.Type.members.Payback,
        sourceId = payback.sourceId.value,
        amount = payback.amount.value,
        description = payback.description.value,
        reportingDate = payback.reportingDate.toLocalDate
      )
  }

  implicit val transactionRequestMapping: Mapping[Transaction, TransactionModel] = (a: Transaction) =>
    TransactionModel(
      id = Some(Id(a.id)),
      description = Description(a.description),
      transactionDate = OffsetDateTime.of(a.transactionDate, LocalTime.MIN, ZoneOffset.UTC),
      accountId = Id(a.accountId),
      amounts = a.amounts.map(_.mapTo[AmountModel])
    )

  implicit val transactionResponseMapping: Mapping[TransactionModel, Transaction] = (a: TransactionModel) =>
    Transaction(
      id = a.id.map(_.value).getOrElse(-1),
      description = a.description.value,
      transactionDate = a.transactionDate.toLocalDate,
      accountId = a.accountId.value,
      amounts = a.amounts.map(_.mapTo[Amount]).toVector
    )
}
