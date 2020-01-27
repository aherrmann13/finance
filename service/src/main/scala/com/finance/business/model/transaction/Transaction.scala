package com.finance.business.model.transaction

import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Transaction(
    id: Id,
    description: Description,
    transactionDate: DateTime,
    sourceId: Int,
    accountId: Int,
    amounts: Seq[Amount]
)
