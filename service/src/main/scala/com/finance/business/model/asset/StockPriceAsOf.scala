package com.finance.business.model.asset

import java.time.OffsetDateTime

import com.finance.business.model.types.Usd

// TODO: correct place?
case class StockPriceAsOf(open: Usd, current: Usd, asOf: OffsetDateTime)
