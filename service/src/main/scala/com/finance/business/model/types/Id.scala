package com.finance.business.model.types

object Id {
  def apply(value: Int): Id = new Id(value)
}

class Id(val value: Int) extends AnyVal;
