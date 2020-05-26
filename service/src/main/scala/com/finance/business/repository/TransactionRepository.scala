package com.finance.business.repository

import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types.{DateRange, Id}
import com.finance.business.repository.query.TransactionQuery

trait TransactionRepository[F[_]] extends Repository[F, Transaction] {
  def get(query: TransactionQuery): F[Seq[Transaction]]

  def anyWithAccountId(accountId: Id): F[Boolean]

  def anyWithPaybackId(paybackId: Id): F[Boolean]

  def anyWithCategoryId(sourceId: Id): F[Boolean]

  def anyWithSourceId(sourceId: Id): F[Boolean]

  // TODO: better method of determining this
  def anyOutsideRanges(categoryId: Id, ranges: Seq[DateRange]): F[Boolean]

  def getCategoryAmountsInRange(range: DateRange): F[Seq[CategoryAmount]]

  def getByPaybackIds(paybackIds: Seq[Id]): F[Seq[PaybackAmount]]

  def getAllPaybacks: F[Seq[PaybackAmount]]
}