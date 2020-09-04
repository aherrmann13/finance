package com.finance.business.model.types

import cats.implicits._
import com.finance.business.instances.NumericInstances._

object Usd {
  def apply(value: BigDecimal): Usd = new Usd(value)

  object implicits {
    implicit def usdNumeric(implicit numeric: Numeric[BigDecimal]): Numeric[Usd] = numeric.imap(Usd(_))(_.value)
  }
}

class Usd(val value: BigDecimal) extends AnyVal
