package com.finance.business.validation.errors

import com.finance.business.model.asset.StockAction

sealed trait StockActionsInvalid extends ValidationError

case class NoStockToPayDividend(action: StockAction) extends StockActionsInvalid
case class SellingMoreThanCurrentlyHave(action: StockAction) extends StockActionsInvalid
