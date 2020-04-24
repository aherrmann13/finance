package com.finance.business.model.asset

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

trait Asset extends Model {
  val accountId: Id
}
