package com.finance.business.services.query

import java.time.OffsetDateTime

import com.finance.business.model.types.Id

case class RecordQuery(
  to: Option[OffsetDateTime] = None,
  from: Option[OffsetDateTime] = None,
  accountIds: Set[Id] = Set.empty
)
