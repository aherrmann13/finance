package com.finance.service.config

import cats.effect.Async
import cats.implicits._
import com.finance.business.services._
import com.finance.business.validation._
import com.finance.persistence.fakes._
import com.finance.service.endpoints.account.AccountResource
import com.finance.service.endpoints.asset.AssetResource
import com.finance.service.endpoints.category.CategoryResource
import com.finance.service.endpoints.payback.PaybackResource
import com.finance.service.endpoints.record.RecordResource
import com.finance.service.endpoints.reporting.ReportingResource
import com.finance.service.endpoints.source.SourceResource
import com.finance.service.endpoints.transaction.TransactionResource
import com.finance.service.endpoints.transfer.TransferResource
import com.finance.service.handlers._
import org.http4s.HttpApp
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT

object HttpAppBuilder {
  def build[F[_]: Async]: HttpApp[F] = {
    val accountRepository = new FakeAccountRepository[F]
    val assetRepository = new FakeAssetRepository[F]
    val categoryRepository = new FakeCategoryRepository[F]
    val paybackRepository = new FakePaybackRepository[F]
    val sourceRepository = new FakeSourceRepository[F]
    val transactionRepository = new FakeTransactionRepository[F]
    val transferRepository = new FakeTransferRepository[F]

    val stockPriceRetriever = new FakeStockPriceRetriever[F]

    val accountValidationInterpreter =
      new AccountValidationInterpreter(accountRepository, transactionRepository, assetRepository, paybackRepository)
    val accountService = new AccountService(accountValidationInterpreter, accountRepository)

    val assetValidationInterpreter = new AssetValidationInterpreter(assetRepository, accountRepository)
    val assetService = new AssetService(assetValidationInterpreter, assetRepository, stockPriceRetriever)

    val categoryValidationInterpreter = new CategoryValidationInterpreter(categoryRepository, transactionRepository)
    val categoryService = new CategoryService(categoryValidationInterpreter, categoryRepository, transactionRepository)

    val paybackValidationInterpreter = new PaybackValidationInterpreter(paybackRepository, transactionRepository)
    val paybackService = new PaybackService(paybackValidationInterpreter, paybackRepository, transactionRepository)

    val recordService = new RecordService(assetRepository, transferRepository, transactionRepository)

    val reportingService = new ReportingService(
      accountRepository,
      transactionRepository,
      assetRepository,
      stockPriceRetriever
    )

    val sourceValidationInterpreter = new SourceValidationInterpreter(sourceRepository, transactionRepository)
    val sourceService = new SourceService(sourceValidationInterpreter, sourceRepository)

    val transactionValidationInterpreter = new TransactionValidationInterpreter(
      transactionRepository,
      sourceRepository,
      accountRepository,
      categoryRepository,
      paybackRepository
    )
    val transactionService = new TransactionService(transactionValidationInterpreter, transactionRepository)

    val transferValidationInterpreter = new TransferValidationInterpreter(transferRepository, accountRepository)
    val transferService = new TransferService(transferValidationInterpreter, transferRepository)

    (
      new AccountResource().routes(new AccountHandlerImpl(accountService)) combineK
        new AssetResource().routes(new AssetHandlerImpl(assetService)) combineK
        new CategoryResource().routes(new CategoryHandlerImpl(categoryService)) combineK
        new PaybackResource().routes(new PaybackHandlerImpl(paybackService)) combineK
        new RecordResource().routes(new RecordHandlerImpl(recordService)) combineK
        new ReportingResource().routes(new ReportingHandlerImpl(reportingService)) combineK
        new SourceResource().routes(new SourceHandlerImpl(sourceService)) combineK
        new TransactionResource().routes(new TransactionHandlerImpl(transactionService)) combineK
        new TransferResource().routes(new TransferHandlerImpl(transferService))
    ).orNotFound
  }
}
