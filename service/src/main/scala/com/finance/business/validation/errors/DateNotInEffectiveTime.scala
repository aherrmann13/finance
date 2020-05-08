package com.finance.business.validation.errors

import com.finance.business.model.category.EffectiveTime
import com.github.nscala_time.time.Imports._

case class DateNotInEffectiveTime(date: DateTime, time: EffectiveTime) extends ValidationError
