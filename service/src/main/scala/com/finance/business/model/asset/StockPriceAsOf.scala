package com.finance.business.model.asset

import com.finance.business.model.types.Usd
import com.github.nscala_time.time.Imports._

// TODO: correct place?
case class StockPriceAsOf(open: Usd, current: Usd, asOf: DateTime)
