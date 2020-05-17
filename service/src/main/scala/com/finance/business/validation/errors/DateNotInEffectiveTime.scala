package com.finance.business.validation.errors

import com.finance.business.model.types.DateRange
import com.github.nscala_time.time.Imports._

case class DateNotInEffectiveTime(date: DateTime, ranges: Seq[DateRange]) extends ValidationError
