package com.finance.business.model.transfer

import com.finance.business.common.{HasId, HasUserId}
import com.github.nscala_time.time.Imports._

case class Transfer(
  id: Option[Int],
  userId: Int,
  to: Int,
  from: Int,
  amount: BigDecimal,
  date: DateTime
) extends HasId with HasUserId