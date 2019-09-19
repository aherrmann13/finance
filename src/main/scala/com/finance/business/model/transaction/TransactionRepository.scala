package com.finance.business.model.transaction

import com.finance.business.common.Repository

trait TransactionRepository[F[_]] extends Repository[F, Transaction] {
  def anyWithAccountId(userId: Int, id: Int): F[Boolean]

  def anyWithSourceId(userId: Int, id: Int): F[Boolean]
}