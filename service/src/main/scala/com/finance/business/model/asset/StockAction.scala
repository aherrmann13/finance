package com.finance.business.model.asset

import com.finance.business.model.types.Usd
import com.github.nscala_time.time.Imports._

case class StockAction(
    date: DateTime,
    actionType: StockActionType,
    units: BigDecimal,
    unitPrice: Usd,
    amountPaid: Usd
)