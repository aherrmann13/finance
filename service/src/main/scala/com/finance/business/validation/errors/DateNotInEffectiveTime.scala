package com.finance.business.validation.errors

import java.time.OffsetDateTime

import com.finance.business.model.types.DateRange

case class DateNotInEffectiveTime(date: OffsetDateTime, ranges: Seq[DateRange]) extends ValidationError
