package com.finance.business.validation.errors

import com.finance.business.model.types.DateRange

case class BudgetEffectiveTimeNotWithinCategory(budgetEffectiveTime: Seq[DateRange], categoryEffectiveTime: Seq[DateRange])
    extends ValidationError
