package com.finance.service.converters

import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.Error
import com.finance.service.handlers.NullBodyError

object ErrorMapping {
  implicit def validationErrorToResponseMapping[E <: ValidationError]: Mapping[E, Error] = {
    case NullBodyError => Error(message = Some("body is null"))
    case _ => Error(message = Some("unknown error occurred"))
  }
}
