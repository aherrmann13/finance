package com.finance.business.model.asset

import com.finance.business.model.types.Usd

case class StockMetadata(
    ticker: String,
    unitPrice: Usd,
    dividend: Option[Dividend]
)
