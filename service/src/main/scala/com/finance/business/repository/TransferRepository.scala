package com.finance.business.repository

import com.finance.business.model.transfer.Transfer
import com.finance.business.repository.query.TransferQuery

trait TransferRepository[F[_]] extends Repository[F, Transfer] {
  def get(query: TransferQuery): F[Seq[Transfer]]
}
