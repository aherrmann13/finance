package com.finance.business.model.asset

case class StockPurchaseLifecycle(
  stock: Stock,
  buy: Buy,
  lifecycle: Seq[StockAction]
)
