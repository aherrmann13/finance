package com.finance.business.model.transfer

import java.time.OffsetDateTime

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Transfer(
  id: Option[Id],
  from: Id,
  fromDate: OffsetDateTime,
  to: Id,
  toDate: OffsetDateTime,
  amount: Usd
) extends Model
