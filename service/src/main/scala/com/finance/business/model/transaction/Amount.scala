package com.finance.business.model.transaction

import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Amount(categoryId: Id, amount: Usd, description: Description, reportingDate: DateTime)
