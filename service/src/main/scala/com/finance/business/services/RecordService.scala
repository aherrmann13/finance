package com.finance.business.services

import cats.Monad
import cats.implicits._
import com.finance.business.model.asset.Stock
import com.finance.business.model.record.{AssetRecord, Record, TransactionRecord, TransferRecord}
import com.finance.business.model.transaction.Transaction
import com.finance.business.model.transfer.Transfer
import com.finance.business.repository.query.{StockQuery, TransactionQuery, TransferQuery}
import com.finance.business.repository.{AssetRepository, TransactionRepository, TransferRepository}
import com.finance.business.services.query.RecordQuery

class RecordService[F[_]: Monad](
  assetRepository: AssetRepository[F],
  transferRepository: TransferRepository[F],
  transactionRepository: TransactionRepository[F]
) {
  def getRecords(query: RecordQuery): F[Seq[Record]] =
    for {
      stocks <- getStocks(query)
      transfers <- getTransfers(query)
      transactions <- getTransactions(query)
      stockRecords = stocks.map(AssetRecord)
      transferRecords = transfers.map(TransferRecord)
      transactionRecords = transactions.map(TransactionRecord)
    } yield stockRecords ++ transferRecords ++ transactionRecords

  private def getStocks(query: RecordQuery): F[Seq[Stock]] =
    assetRepository.getStocks(
      StockQuery(
        to = query.to,
        from = query.from,
        accountIds = query.accountIds
      )
    )

  private def getTransfers(query: RecordQuery): F[Seq[Transfer]] =
    transferRepository.get(
      TransferQuery(
        to = query.to,
        from = query.from,
        accountIds = query.accountIds
      )
    )

  private def getTransactions(query: RecordQuery): F[Seq[Transaction]] =
    transactionRepository.get(
      TransactionQuery(
        to = query.to,
        from = query.from,
        accountIds = query.accountIds
      )
    )
}
