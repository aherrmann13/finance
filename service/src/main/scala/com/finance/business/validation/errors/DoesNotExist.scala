package com.finance.business.validation.errors

import com.finance.business.model.types.Id

case class DoesNotExist(model: ModelName, id: Option[Id]) extends ValidationError
