package com.finance.business.account

import com.finance.business.common.errors.BusinessError

case object AccountDoesNotExistError extends BusinessError

case object AccountAlreadyExistsError extends BusinessError

case object NameMustBeDefinedError extends BusinessError

case object DescriptionMustBeDefinedError extends BusinessError

case object NameExceedsMaxLengthError extends BusinessError

case object DescriptionExceedsMaxLengthError extends BusinessError