package com.finance.business.model.types

object Name {
  def apply(value: String): Name = new Name(value)
}

class Name(val value: String) extends AnyVal
