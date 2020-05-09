package com.finance.business.model.transaction

import com.finance.business.model.base.Model
import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Transaction(
    id: Option[Id],
    description: Description,
    transactionDate: DateTime,
    accountId: Id,
    amounts: Seq[Amount]
) extends Model
