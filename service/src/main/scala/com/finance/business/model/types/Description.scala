package com.finance.business.model.types

object Description {
  def apply(value: String): Description = new Description(value)
}
class Description(val value: String) extends AnyVal
