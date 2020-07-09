package com.finance.business.services

import java.time.OffsetDateTime

import cats.Monad
import cats.implicits._
import cats.kernel.Order
import com.finance.business.model.reporting.AccountValue
import com.finance.business.model.transaction.{CategoryAmount, Transaction}
import com.finance.business.model.types.{DateRange, Id, Usd}
import com.finance.business.operations.CategoryOps._
import com.finance.business.operations.StockOps._
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.query.TransactionQuery
import com.finance.business.repository.{AccountRepository, AssetRepository, TransactionRepository}
import com.finance.business.services.query.AccountValueQuery

object ReportingService {
  private case class AccountValueItem(account: Id, date: OffsetDateTime, value: Usd)

  private def toAccountValueItems(useReportingDate: Boolean, transactions: Seq[Transaction]): Seq[AccountValueItem] =
    for {
      transaction <- transactions
      amount <- transaction.amounts.collect { case c: CategoryAmount => c }
      date = if (useReportingDate) amount.reportingDate else transaction.transactionDate
    } yield AccountValueItem(transaction.accountId, date, amount.amount)

  // TODO: right spot for these?
  // these are here so we can use `groupByNel` to get a non empty in the group by
  private implicit val dateTimeOrder: Order[OffsetDateTime] = Order.by[OffsetDateTime, Long](_.toInstant.getEpochSecond)
  private implicit val dateRangeAcctTupleOrder: Order[(DateRange, Id)] =
    Order.by[(DateRange, Id), (Int, OffsetDateTime, OffsetDateTime)](t => (t._2.value, t._1.start, t._1.end))
}

class ReportingService[F[_]: Monad](
  accountRepository: AccountRepository[F],
  transactionRepository: TransactionRepository[F],
  assetRepository: AssetRepository[F],
  stockPriceRetriever: StockPriceRetriever[F]
) {
  import ReportingService._

  def getNetWorth: F[Usd] =
    for {
      accounts <- accountRepository.getAll
      transactions <- transactionRepository.getAll
      stocks <- assetRepository.getAllStocks
      stockValues <- stocks.toList.traverse { stock =>
        stockPriceRetriever.call(stock.ticker).map(p => stock.actions valueWithPrice p.current)
      }
      existingAccountValue = Usd(accounts.map(_.initialAmount.value).sum)
      transactionValue =
        transactions
          .flatMap(_.amounts)
          .collect { case c: CategoryAmount => c }
          .map(_.amount)
          .reduceOption((x, y) => Usd(x.value + y.value))
          .getOrElse(Usd(0))
      stockValue = stockValues.reduceOption((x, y) => Usd(x.value + y.value)).getOrElse(Usd(0))
    } yield Usd(existingAccountValue.value + transactionValue.value + stockValue.value)

  def getAccountValue(query: AccountValueQuery): F[Seq[AccountValue]] =
    for {
      transactions <- getTransactionAmounts(query)
      stocks <- getStockAmounts(query)
      group = (transactions ++ stocks).toList.groupByNel(v => (v.dateRange, v.accountId))
      merged = group.toSeq.map { entry =>
        AccountValue(entry._1._1, entry._1._2, Usd(entry._2.map(_.value.value).reduce))
      }
    } yield merged

  private def getTransactionAmounts(query: AccountValueQuery): F[Seq[AccountValue]] =
    query.dateRanges.toList.flatTraverse { dateRange =>
      transactionRepository
        .get(
          TransactionQuery(
            from = Some(dateRange.start),
            to = Some(dateRange.end),
            accountIds = query.accountIds,
            useReportingDate = query.useReportingDate
          )
        )
        .map(_.toList)
    } map { transactions =>
      for {
        range <- query.dateRanges
        acctId <- query.accountIds
        filteredAmounts = toAccountValueItems(query.useReportingDate.getOrElse(false), transactions)
          .filter(a => a.account == acctId && range.contains(a.date))
        price = filteredAmounts.map(_.value).reduceOption((x, y) => Usd(x.value + y.value)).getOrElse(Usd(0))
      } yield AccountValue(range, acctId, price)
    }

  private def getStockAmounts(query: AccountValueQuery): F[Seq[AccountValue]] =
    query.countAssetGrowthInPurchaseMonth
      .getOrElse(false)
      .pure[F]
      .ifM(
        ifTrue = getStockValueWithAssetGrowthInPurchaseMonth(query),
        ifFalse = getStockAmountValueAtMonthEnd(query)
      )

  private def getStockValueWithAssetGrowthInPurchaseMonth(query: AccountValueQuery): F[Seq[AccountValue]] =
    assetRepository.getAllStocks flatMap { stocks =>
      stocks
        .flatMap { stock =>
          stock.asLifecycle
        }
        .filter { lifecycle =>
          query.dateRanges.exists(range => range.contains(lifecycle.buy.date))
        }
        .toList
        .traverse { lifecycle =>
          stockPriceRetriever.call(lifecycle.stock.ticker).map((_, lifecycle))
        }
    } map { lifecyclesWithPrice =>
      for {
        range <- query.dateRanges
        acctId <- query.accountIds
        lifecycles = lifecyclesWithPrice.filter(lf => lf._2.stock.accountId == acctId && range.contains(lf._2.buy.date))
        price =
          lifecycles
            .map(lf => lf._2 valueWithPrice lf._1.current)
            .reduceOption((x, y) => Usd(x.value + y.value))
            .getOrElse(Usd(0))
      } yield AccountValue(range, acctId, price)
    }

  private def getStockAmountValueAtMonthEnd(query: AccountValueQuery): F[Seq[AccountValue]] =
    assetRepository.getAllStocks flatMap { stocks =>
      stocks.toList.flatTraverse { stock =>
        query.dateRanges.toList.traverse { range =>
          for {
            startingUnitPrice <- stockPriceRetriever.call(stock.ticker, range.start)
            endingUnitPrice <- stockPriceRetriever.call(stock.ticker, range.end)
            startingTotalPrice = stock.actions.filter(_.date <= range.start) valueWithPrice startingUnitPrice.current
            endingTotalPrice = stock.actions.filter(_.date <= range.end) valueWithPrice endingUnitPrice.current
          } yield AccountValue(range, stock.accountId, Usd(endingTotalPrice.value - startingTotalPrice.value))
        }
      }
    } map (_.toSeq)
}
