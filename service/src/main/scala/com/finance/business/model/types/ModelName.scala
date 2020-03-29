package com.finance.business.model.types

object ModelName {
  def apply(value: String): ModelName = new ModelName(value)
}

class ModelName(val value: String) extends AnyVal
