package com.finance.business.model.types

object Id {
  def apply(id: Int): Id = new Id(id)
}

class Id(val id: Int) extends AnyVal;
