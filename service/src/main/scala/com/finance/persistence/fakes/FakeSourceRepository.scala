package com.finance.persistence.fakes

import cats.Monad
import cats.implicits._
import com.finance.business.model.source.Source
import com.finance.business.repository.SourceRepository

class FakeSourceRepository[F[_]: Monad] extends FakeRepository[F, Source] with SourceRepository[F] {
  override def getFuzzyMatch(fuzzy: String): F[Seq[Source]] = Seq.empty[Source].pure[F]
}
