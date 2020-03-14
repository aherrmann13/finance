package com.finance.business.repository

import com.finance.business.model.payback.Payback
import com.finance.business.model.types.Id

trait PaybackRepository[F[_]] extends Repository[F, Payback] {
  def anyWithAccountId(accountId: Id): F[Boolean]
}