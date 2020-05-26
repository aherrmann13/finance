package com.finance.business.model.asset

import com.finance.business.model.types.Usd
import com.github.nscala_time.time.Imports._

sealed trait StockAction {
  val date: DateTime
  val units: BigDecimal
  val unitPrice: Usd
  val amount: Usd
}

case class Buy(date: DateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class LifoSell(date: DateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class FifoSell(date: DateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class CashDividend(date: DateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class StockDividend(date: DateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
