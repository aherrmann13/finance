package com.finance.business.model.transfer

import com.finance.business.model.types.{HasId, ModelName, NamedModel}

object implicits {
  private val transferModelName = ModelName("Transfer")

  implicit val transferHasId: HasId[Transfer] = (target: Transfer) => target.id
  implicit val transferHasModelName: NamedModel[Transfer] = new NamedModel[Transfer] {
    override def modelName: ModelName = transferModelName
  }
}
