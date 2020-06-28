package com.finance.business.services.query

import com.finance.business.model.types.Id
import com.github.nscala_time.time.Imports.DateTime

case class RecordQuery(
  to: Option[DateTime] = None,
  from: Option[DateTime] = None,
  accountIds: Set[Id] = Set.empty,
)