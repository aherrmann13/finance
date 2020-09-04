package com.finance.service.converters

import com.finance.business.model.reporting.{AccountBalance => AccountBalanceModel, AccountValue => AccountValueModel}
import com.finance.business.model.types.{Id, DateRange => DateRangeModel}
import com.finance.business.services.query.{AccountValueQuery => AccountValueQueryModel}
import com.finance.service.converters.CommonMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{AccountBalance, AccountBalanceQuery, AccountValue, AccountValueQuery}

object ReportingMapping {

  implicit val accountValueQueryRequestMapping: Mapping[AccountValueQuery, AccountValueQueryModel] =
    (a: AccountValueQuery) =>
      AccountValueQueryModel(
        dateRanges = a.dateRanges.map(_.mapTo[DateRangeModel]),
        accountIds = a.accountIds.map(Id(_)).toSet,
        useReportingDate = a.useReportingDate,
        countAssetGrowthInPurchaseMonth = a.countAssetGrowthInPurchaseMonth
      )

  implicit val accountValueResponseMapping: Mapping[AccountValueModel, AccountValue] = (a: AccountValueModel) =>
    AccountValue(
      accountId = a.accountId.value,
      dateRange = a.dateRange.mapTo[AccountValue.DateRange],
      value = a.value.value
    )

  implicit val accountBalanceQueryRequestMapping: Mapping[AccountBalanceQuery, Set[Id]] =
    (a: AccountBalanceQuery) => a.accountIds.map(x => Id(x)).toSet

  implicit val accountBalanceResponseMapping: Mapping[AccountBalanceModel, AccountBalance] =
    (a: AccountBalanceModel) =>
      AccountBalance(
        accountId = a.accountId.value,
        balance = a.balance.value
      )
}
