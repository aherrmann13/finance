package com.finance.business.repository

import com.finance.business.model.source.Source

trait SourceRepository[F[_]] extends Repository[F, Source] {
  def getFuzzyMatch(fuzzy: String): F[Seq[Source]]
}