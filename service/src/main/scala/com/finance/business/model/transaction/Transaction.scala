package com.finance.business.model.transaction

import java.time.OffsetDateTime

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Transaction(
    id: Option[Id],
    description: Description,
    transactionDate: OffsetDateTime,
    accountId: Id,
    amounts: Seq[Amount]
) extends Model
