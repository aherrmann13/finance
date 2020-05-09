package com.finance.business.model.transaction

import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

sealed trait Amount {
  val amount: Usd
  val sourceId: Id
  val description: Description
  val reportingDate: DateTime
}

case class CategoryAmount(
  categoryId: Id,
  sourceId: Id,
  amount: Usd,
  description: Description,
  reportingDate: DateTime
) extends Amount

case class PaybackAmount(
  paybackId: Id,
  sourceId: Id,
  amount: Usd,
  description: Description,
  reportingDate: DateTime
) extends Amount
