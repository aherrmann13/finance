package com.finance.business.source

sealed trait SourceValidationError

case object SourceDoesNotExistError extends SourceValidationError
case object SourceAlreadyExistsError extends SourceValidationError
case object NameMustBeDefinedError extends SourceValidationError
case object DescriptionMustBeDefinedError extends SourceValidationError
case object NameExceedsMaxLengthError extends SourceValidationError
case object DescriptionExceedsMaxLengthError extends SourceValidationError