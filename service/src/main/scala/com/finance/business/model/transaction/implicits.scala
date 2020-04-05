package com.finance.business.model.transaction

import com.finance.business.model.types._

object implicits {
  private val transactionModelName = ModelName("Transaction")
  private val amountModelName = ModelName("Amount")

  implicit val transactionHasId: HasId[Transaction] = (target: Transaction) => target.id
  implicit val transactionHasDescription: HasDescription[Transaction] = (target: Transaction) => target.description
  implicit val transactionHasModelName: NamedModel[Transaction] = new NamedModel[Transaction] {
    override def modelName: ModelName = transactionModelName
  }

  implicit val amountHasDescription: HasDescription[Amount] = (target: Amount) => target.description
  implicit val amountHasAmountModelName: NamedModel[Amount] = new NamedModel[Amount] {
    override def modelName: ModelName = amountModelName
  }
}
