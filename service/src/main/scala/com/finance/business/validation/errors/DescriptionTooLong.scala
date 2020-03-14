package com.finance.business.validation.errors

import com.finance.business.model.types.Description

case class DescriptionTooLong(modelName: ModelName, description: Description)
