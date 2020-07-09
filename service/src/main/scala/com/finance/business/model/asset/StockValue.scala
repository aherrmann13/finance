package com.finance.business.model.asset

import java.time.OffsetDateTime

import com.finance.business.model.types.Usd

// TODO: correct place?
case class StockValue(
  stock: Stock,
  price: Usd,
  asOf: OffsetDateTime,
  quantity: BigDecimal,
  daysChange: Usd,
  daysChangePercentage: BigDecimal,
  daysGain: Usd,
  pricePaid: Usd,
  totalGain: Usd,
  value: Usd
)
