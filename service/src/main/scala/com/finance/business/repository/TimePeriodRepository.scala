package com.finance.business.repository

import com.finance.business.model.timeperiod.TimePeriodRange
import com.finance.business.model.types.Id

trait TimePeriodRepository[F[_]] {
  def getMany(ids: Seq[Id]): F[Seq[TimePeriodRange]]
}
