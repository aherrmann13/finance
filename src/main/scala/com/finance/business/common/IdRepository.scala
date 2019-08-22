package com.finance.business.common

// TODO: better name
trait IdRepository[F[_]] {
  def userExists(id: Int): F[Boolean]
}
