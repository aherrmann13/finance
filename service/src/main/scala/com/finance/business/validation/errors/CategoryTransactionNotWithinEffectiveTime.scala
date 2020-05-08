package com.finance.business.validation.errors

import com.finance.business.model.category.EffectiveTime

case class CategoryTransactionNotWithinEffectiveTime(time: EffectiveTime) extends ValidationError
