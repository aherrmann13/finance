package com.finance.business.model.types

object Description {
  def apply(description: String): Description = new Description(description)
}
class Description(val description: String) extends AnyVal
