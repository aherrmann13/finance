package com.finance.business.repository.query

import com.finance.business.model.types.Id
import com.github.nscala_time.time.Imports._

case class StockQuery(
    ticker: Option[String] = None,
    to: Option[DateTime] = None,
    from: Option[DateTime] = None,
    accountIds: Set[Id] = Set.empty
)
