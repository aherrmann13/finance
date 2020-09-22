package com.finance.persistence.fakes

import cats.Monad
import cats.implicits._
import com.finance.business.model.transfer.Transfer
import com.finance.business.repository.TransferRepository
import com.finance.business.repository.query.TransferQuery

class FakeTransferRepository[F[_]: Monad] extends FakeRepository[F, Transfer] with TransferRepository[F] {
  override def get(query: TransferQuery): F[Seq[Transfer]] = Seq.empty[Transfer].pure[F]
}
