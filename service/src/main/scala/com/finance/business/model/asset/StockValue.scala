package com.finance.business.model.asset

import com.finance.business.model.types.Usd
import org.joda.time.DateTime

// TODO: correct place?
case class StockValue(
    stock: Stock,
    price: Usd,
    asOf: DateTime,
    quantity: BigDecimal,
    daysChange: Usd,
    daysChangePercentage: BigDecimal,
    daysGain: Usd,
    pricePaid: Usd,
    totalGain: Usd,
    value: Usd
)
