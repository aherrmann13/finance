package com.finance.persistence.fakes

import cats.Monad
import cats.implicits._
import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types.{DateRange, Id}
import com.finance.business.repository.TransactionRepository
import com.finance.business.repository.query.TransactionQuery

class FakeTransactionRepository[F[_]: Monad] extends FakeRepository[F, Transaction] with TransactionRepository[F] {
  override def get(query: TransactionQuery): F[Seq[Transaction]] = Seq.empty[Transaction].pure[F]

  override def anyWithAccountId(accountId: Id): F[Boolean] = true.pure[F]

  override def anyWithPaybackId(paybackId: Id): F[Boolean] = true.pure[F]

  override def anyWithCategoryId(sourceId: Id): F[Boolean] = true.pure[F]

  override def anyWithSourceId(sourceId: Id): F[Boolean] = true.pure[F]

  override def anyOutsideRanges(categoryId: Id, ranges: Seq[DateRange]): F[Boolean] = true.pure[F]

  override def getCategoryAmountsInRange(range: DateRange): F[Seq[CategoryAmount]] = Seq.empty[CategoryAmount].pure[F]

  override def getByPaybackIds(paybackIds: Seq[Id]): F[Seq[PaybackAmount]] = Seq.empty[PaybackAmount].pure[F]

  override def getAllPaybacks: F[Seq[PaybackAmount]] = Seq.empty[PaybackAmount].pure[F]
}
