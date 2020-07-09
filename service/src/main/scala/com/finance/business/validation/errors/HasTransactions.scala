package com.finance.business.validation.errors

import com.finance.business.model.types.ModelName

case class HasTransactions(model: ModelName) extends ValidationError
