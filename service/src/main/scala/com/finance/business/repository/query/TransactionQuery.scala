package com.finance.business.repository.query

import java.time.OffsetDateTime

import com.finance.business.model.types.{Id, Usd}

case class TransactionQuery(
  to: Option[OffsetDateTime] = None,
  from: Option[OffsetDateTime] = None,
  accountIds: Set[Id] = Set.empty,
  sourceIds: Set[Id] = Set.empty,
  lessThan: Option[Usd] = None,
  greaterThan: Option[Usd] = None,
  useReportingDate: Option[Boolean] = None
)
