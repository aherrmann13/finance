package com.finance.business.model.asset

import com.finance.business.model.types._

object implicits {
  private val assetModelName = ModelName("Asset")

  implicit val hasId: HasId[Asset] = (target: Asset) => target.id
  implicit val hasModelName: NamedModel[Asset] = new NamedModel[Asset] {
    override def modelName: ModelName = assetModelName
  }
}
