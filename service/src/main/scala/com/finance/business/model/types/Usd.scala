package com.finance.business.model.types

object Usd {
  def apply(value: BigDecimal): Usd = new Usd(value)
}

class Usd(val value: BigDecimal) extends AnyVal
