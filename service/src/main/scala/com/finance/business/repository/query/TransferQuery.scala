package com.finance.business.repository.query

import java.time.OffsetDateTime

import com.finance.business.model.types.{Id, Usd}

case class TransferQuery(
  to: Option[OffsetDateTime] = None,
  from: Option[OffsetDateTime] = None,
  accountIds: Set[Id] = Set.empty,
  lessThan: Option[Usd] = None,
  greaterThan: Option[Usd] = None
)
