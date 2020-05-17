package com.finance.business.validation.errors

import com.finance.business.model.types.DateRange

case class CategoryEffectiveTimeNotWithinParent(effectiveTime: Seq[DateRange], parentEffectiveTime: Seq[DateRange])
    extends ValidationError
