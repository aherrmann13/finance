package com.finance.business.services

import java.time.OffsetDateTime

import cats.Monad
import cats.implicits._
import cats.kernel.Order
import com.finance.business.model.reporting.{AccountBalance, AccountValue}
import com.finance.business.model.transaction.{CategoryAmount, Transaction}
import com.finance.business.model.types.Usd.implicits._
import com.finance.business.model.types.{DateRange, Id, Usd}
import com.finance.business.operations.CategoryOps._
import com.finance.business.operations.StockOps._
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.query.{StockQuery, TransactionQuery}
import com.finance.business.repository.{AccountRepository, AssetRepository, TransactionRepository}
import com.finance.business.services.query.AccountValueQuery

import scala.Numeric.Implicits._

object ReportingService {
  private case class AccountValueItem(account: Id, date: OffsetDateTime, value: Usd)
  private case class AccountItem(accountId: Id, dateRange: DateRange)

  private def toAccountValueItems(useReportingDate: Boolean, transactions: Seq[Transaction]): Seq[AccountValueItem] =
    for {
      transaction <- transactions
      amount <- transaction.amounts.collect { case c: CategoryAmount => c }
      date = if (useReportingDate) amount.reportingDate else transaction.transactionDate
    } yield AccountValueItem(transaction.accountId, date, amount.amount)
  // needed for <= on datetime
  private implicit val dateTimeOrder: Order[OffsetDateTime] = Order.by[OffsetDateTime, Long](_.toInstant.getEpochSecond)

}

class ReportingService[F[_]: Monad](
  accountRepository: AccountRepository[F],
  transactionRepository: TransactionRepository[F],
  assetRepository: AssetRepository[F],
  stockPriceRetriever: StockPriceRetriever[F]
) {
  import ReportingService._

  def getNetWorth(asOf: OffsetDateTime): F[Usd] =
    for {
      accounts <- accountRepository.getAll
      transactions <- transactionRepository.get(TransactionQuery(to = Some(asOf)))
      stocks <- assetRepository.getStocks(StockQuery(to = Some(asOf)))
      stockValues <- stocks.toList.traverse { stock =>
        stockPriceRetriever.call(stock.ticker, asOf).map(p => stock.actions valueWithPrice p.current)
      }
      existingAccountValue = Usd(accounts.map(_.initialAmount.value).sum)
      transactionValue =
        transactions
          .flatMap(_.amounts)
          .collect { case c: CategoryAmount => c.amount }
          .sum
      stockValue = stockValues.sum
    } yield existingAccountValue + transactionValue + stockValue

  def getAccountValue(query: AccountValueQuery, currentDateTime: OffsetDateTime): F[Seq[AccountValue]] =
    for {
      transactions <- getTransactionAmounts(query)
      stocks <- getStockAmounts(query, currentDateTime)
      group = (transactions ++ stocks).groupBy(v => AccountItem(v.accountId, v.dateRange))
      merged = group.map(entry => AccountValue(entry._1.dateRange, entry._1.accountId, entry._2.map(_.value).sum)).toSeq
    } yield merged

  def getAccountBalance(accountIds: Set[Id], currentDateTime: OffsetDateTime): F[Seq[AccountBalance]] =
    for {
      transactions <- transactionRepository.get(TransactionQuery(accountIds = accountIds))
      stocks <- assetRepository.getStocks(StockQuery(accountIds = accountIds))
      transactionValues = transactions.map(x => (x.accountId, Usd(x.amounts.map(_.amount.value).sum)))
      stockValues <- stocks.toList.traverse { s =>
        stockPriceRetriever
          .call(s.ticker, currentDateTime)
          .map(v => (s.accountId, s.actions valueWithPrice v.current))
      }
    } yield (transactionValues ++ stockValues).groupBy(_._1).map(x => AccountBalance(x._1, x._2.map(_._2).sum)).toList

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
        price = filteredAmounts.map(_.value).sum
      } yield AccountValue(range, acctId, price)
    }

  private def getStockAmounts(query: AccountValueQuery, currentDateTime: OffsetDateTime): F[Seq[AccountValue]] =
    query.countAssetGrowthInPurchaseMonth
      .getOrElse(false)
      .pure[F]
      .ifM(
        ifTrue = getStockValueWithAssetGrowthInPurchaseMonth(query, currentDateTime),
        ifFalse = getStockAmountValueAtMonthEnd(query)
      )

  private def getStockValueWithAssetGrowthInPurchaseMonth(
    query: AccountValueQuery,
    currentDateTime: OffsetDateTime
  ): F[Seq[AccountValue]] =
    assetRepository.getAllStocks flatMap { stocks =>
      stocks.flatMap { stock =>
        stock.asLifecycle
      }.filter { lifecycle =>
        query.dateRanges.exists(range => range.contains(lifecycle.buy.date))
      }.toList.traverse { lifecycle =>
        stockPriceRetriever.call(lifecycle.stock.ticker, currentDateTime).map((_, lifecycle))
      }
    } map { lifecyclesWithPrice =>
      for {
        range <- query.dateRanges
        acctId <- query.accountIds
        lifecycles = lifecyclesWithPrice.filter(lf => lf._2.stock.accountId == acctId && range.contains(lf._2.buy.date))
        price = lifecycles.map(lf => lf._2 valueWithPrice lf._1.current).sum
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
