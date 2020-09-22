package com.finance.persistence.fakes

import cats.Monad
import cats.implicits._
import com.finance.business.model.payback.Payback
import com.finance.business.model.types.{DateRange, Id}
import com.finance.business.repository.PaybackRepository

class FakePaybackRepository[F[_]: Monad] extends FakeRepository[F, Payback] with PaybackRepository[F] {
  override def anyWithAccountId(accountId: Id): F[Boolean] = true.pure[F]

  override def getInRange(range: DateRange): F[Seq[Payback]] = Seq.empty[Payback].pure[F]
}
