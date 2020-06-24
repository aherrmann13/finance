package com.finance.business.services.query

import com.finance.business.model.types.{DateRange, Id}

case class AccountValueQuery(
  dateRanges: Seq[DateRange],
  accountIds: Set[Id],
  useReportingDate: Option[Boolean],
  countAssetGrowthInPurchaseMonth: Option[Boolean]
)
