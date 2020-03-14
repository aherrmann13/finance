package com.finance.business.validation.errors

import com.finance.business.model.asset.StockAction

sealed trait StockActionsInvalid extends ValidationError

case class DividendMustHavePreviousPurchase(dividend: StockAction) extends StockActionsInvalid


