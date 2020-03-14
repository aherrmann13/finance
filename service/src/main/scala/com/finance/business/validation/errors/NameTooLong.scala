package com.finance.business.validation.errors

import com.finance.business.model.types.Name

case class NameTooLong(modelName: ModelName, name: Name)