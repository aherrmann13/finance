package com.finance.business.model.asset

import com.finance.business.common.{HasId, HasUserId}
import com.github.nscala_time.time.Imports._

case class Asset(
  id: Option[Int],
  userId: Int,
  accountId: Int,
  units: Int,
  amount: BigDecimal,
  date: DateTime,
  identifier: Identifier
) extends HasId with HasUserId