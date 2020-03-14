package com.finance.business.validation.errors

import com.finance.business.model.category.EffectiveTime

case class CategoryNotWithinParentTimePeriod(timePeriod: EffectiveTime, parentTimePeriod: EffectiveTime)
    extends ValidationError
