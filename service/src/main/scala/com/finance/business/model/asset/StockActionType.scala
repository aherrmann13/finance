package com.finance.business.model.asset

sealed trait StockActionType

case object Buy extends StockActionType
case object LifoSell extends StockActionType
case object FifoSell extends StockActionType
case object CashDividend extends StockActionType
case object StockDividend extends StockActionType