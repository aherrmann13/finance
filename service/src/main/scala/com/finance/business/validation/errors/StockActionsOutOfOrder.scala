package com.finance.business.validation.errors

import com.finance.business.model.asset.StockAction

case class StockActionsOutOfOrder(action: StockAction) extends ValidationError
