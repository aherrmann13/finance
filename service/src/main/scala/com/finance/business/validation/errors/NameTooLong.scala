package com.finance.business.validation.errors

import com.finance.business.model.types.{ModelName, Name}

case class NameTooLong(modelName: ModelName, name: Name) extends ValidationError