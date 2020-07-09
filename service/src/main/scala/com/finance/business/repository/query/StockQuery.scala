package com.finance.business.repository.query

import java.time.OffsetDateTime

import com.finance.business.model.types.Id

case class StockQuery(
  ticker: Option[String] = None,
  to: Option[OffsetDateTime] = None,
  from: Option[OffsetDateTime] = None,
  accountIds: Set[Id] = Set.empty
)
