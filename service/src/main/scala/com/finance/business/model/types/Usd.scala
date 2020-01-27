package com.finance.business.model.types

object Usd {
  def apply(amount: BigDecimal): Usd = new Usd(amount)
}

class Usd(val amount: BigDecimal) extends AnyVal