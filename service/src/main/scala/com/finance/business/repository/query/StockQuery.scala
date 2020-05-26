package com.finance.business.repository.query

import com.finance.business.model.types.Id
import com.github.nscala_time.time.Imports._

case class StockQuery(
    ticker: Option[String],
    to: Option[DateTime],
    from: Option[DateTime],
    accountIds: Set[Id]
)
