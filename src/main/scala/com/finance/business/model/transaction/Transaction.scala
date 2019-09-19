package com.finance.business.model.transaction

import com.finance.business.common.{HasId, HasUserId}
import com.github.nscala_time.time.Imports._

case class Transaction(
  id: Option[Int],
  userId: Int,
  amount: Seq[Amount],
  description: String,
  transactionDate: DateTime,
  reportingDate: DateTime,
  sourceId: Int,
  accountId: Int,
  categoryId: Int
) extends HasId with HasUserId