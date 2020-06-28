package com.finance.business.model.record

import com.finance.business.model.asset.Asset
import com.finance.business.model.transaction.Transaction
import com.finance.business.model.transfer.Transfer

sealed trait Record

case class AssetRecord(value: Asset) extends Record
case class TransferRecord(value: Transfer) extends Record
case class TransactionRecord(value: Transaction) extends Record
