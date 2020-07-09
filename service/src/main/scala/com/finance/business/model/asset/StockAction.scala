package com.finance.business.model.asset

import java.time.OffsetDateTime

import com.finance.business.model.types.Usd

sealed trait StockAction {
  val date: OffsetDateTime
  val units: BigDecimal
  val unitPrice: Usd
  val amount: Usd
}

case class Buy(date: OffsetDateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class LifoSell(date: OffsetDateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class FifoSell(date: OffsetDateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class CashDividend(date: OffsetDateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
case class StockDividend(date: OffsetDateTime, units: BigDecimal, unitPrice: Usd, amount: Usd) extends StockAction
