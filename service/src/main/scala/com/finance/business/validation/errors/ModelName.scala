package com.finance.business.validation.errors

object ModelName {
  def apply(value: String): ModelName = new ModelName(value)
}

class ModelName(val value: String) extends AnyVal
