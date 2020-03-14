package com.finance.business.validation.errors

import com.finance.business.model.category.EffectiveTime

case class BudgetPeriodNotInEffectiveTime(budgetEffectiveTime: EffectiveTime, categoryEffectiveTime: EffectiveTime)
    extends ValidationError
