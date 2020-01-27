package com.finance.business.model.asset

case class StockMetadata(
    ticker: String,
    dividend: Option[Dividend]
)
