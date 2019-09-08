package com.finance.business.errors

trait BusinessError

case object UserDoesNotExistError extends BusinessError

case object SourceDoesNotExistError extends BusinessError

case object SourceAlreadyExistsError extends BusinessError

case object AccountDoesNotExistError extends BusinessError

case object AccountAlreadyExistsError extends BusinessError

case object NameMustBeDefinedError extends BusinessError

case object DescriptionMustBeDefinedError extends BusinessError

case object NameExceedsMaxLengthError extends BusinessError

case object DescriptionExceedsMaxLengthError extends BusinessError