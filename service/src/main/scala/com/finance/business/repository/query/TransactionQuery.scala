package com.finance.business.repository.query

import com.finance.business.model.types.{Id, Usd}
import com.github.nscala_time.time.Imports.DateTime

case class TransactionQuery(
  to: Option[DateTime],
  from: Option[DateTime],
  accountIds: Set[Id],
  sourceIds: Set[Id],
  lessThan: Option[Usd],
  greaterThan:  Option[Usd],
  useReportingDate: Option[Boolean]
)