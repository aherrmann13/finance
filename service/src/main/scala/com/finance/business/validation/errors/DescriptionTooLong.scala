package com.finance.business.validation.errors

import com.finance.business.model.types.{Description, ModelName}

case class DescriptionTooLong(modelName: ModelName, description: Description) extends ValidationError
