package com.finance.business.model.types

object Name {
  def apply(name: String): Name = new Name(name)
}

class Name(val name: String) extends AnyVal