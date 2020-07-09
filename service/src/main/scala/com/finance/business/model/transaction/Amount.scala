package com.finance.business.model.transaction

import java.time.OffsetDateTime

import com.finance.business.model.types._

sealed trait Amount {
  val amount: Usd
  val sourceId: Id
  val description: Description
  val reportingDate: OffsetDateTime
}

case class CategoryAmount(
  categoryId: Id,
  sourceId: Id,
  amount: Usd,
  description: Description,
  reportingDate: OffsetDateTime
) extends Amount

case class PaybackAmount(
  paybackId: Id,
  sourceId: Id,
  amount: Usd,
  description: Description,
  reportingDate: OffsetDateTime
) extends Amount
