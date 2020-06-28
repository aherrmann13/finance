package com.finance.business.repository.query

import com.finance.business.model.types.{Id, Usd}
import com.github.nscala_time.time.Imports.DateTime

case class TransferQuery(
  to: Option[DateTime] = None,
  from: Option[DateTime] = None,
  accountIds: Set[Id] = Set.empty,
  lessThan: Option[Usd] = None,
  greaterThan:  Option[Usd] = None
)