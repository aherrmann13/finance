package com.finance.business.repository

import com.finance.business.model.transaction.{PaybackAmount, Transaction}
import com.finance.business.model.types.{DateRange, Id}

trait TransactionRepository[F[_]] extends Repository[F, Transaction] {
  def anyWithAccountId(accountId: Id): F[Boolean]

  def anyWithPaybackId(paybackId: Id): F[Boolean]

  def anyWithCategoryId(sourceId: Id): F[Boolean]

  def anyWithSourceId(sourceId: Id): F[Boolean]

  // TODO: better method of determining this
  def anyOutsideRanges(categoryId: Id, ranges: Seq[DateRange]): F[Boolean]

  // TODO: must be amount range not Transaction range or return 'Amount'
  def getInRange(range: DateRange): F[Seq[Transaction]]

  def getByPaybackIds(paybackIds: Seq[Id]): F[Seq[PaybackAmount]]

  def getAllPaybacks: F[Seq[PaybackAmount]]
}