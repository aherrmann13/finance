package com.finance.business.repository

import com.finance.business.model.transaction.Transaction
import com.finance.business.model.types.Id

trait TransactionRepository[F[_]] extends Repository[F, Transaction] {
  def anyWithAccountId(accountId: Id): F[Boolean]

  def anyWithPaybackId(paybackId: Id): F[Boolean]

  def anyWithCategoryId(sourceId: Id): F[Boolean]

  def anyWithSourceId(sourceId: Id): F[Boolean]
}