package com.finance.business.validation.errors

import com.finance.business.model.types.DateRange

case class TransactionNotWithinBudgetEffectiveTime(time: Seq[DateRange]) extends ValidationError
