package com.finance.business.model.timeperiod

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id
import com.github.nscala_time.time.Imports._

case class TimePeriodRange(id: Option[Id], start: DateTime, end: DateTime) extends Model
