package com.finance.business.source

import com.finance.business.common.errors.BusinessError


case object SourceDoesNotExistError extends BusinessError

case object SourceAlreadyExistsError extends BusinessError

case object NameMustBeDefinedError extends BusinessError

case object DescriptionMustBeDefinedError extends BusinessError

case object NameExceedsMaxLengthError extends BusinessError

case object DescriptionExceedsMaxLengthError extends BusinessError