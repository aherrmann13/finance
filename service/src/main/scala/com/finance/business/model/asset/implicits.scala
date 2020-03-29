package com.finance.business.model.asset

import com.finance.business.model.types._

object implicits {
  private val modelName = ModelName("Asset")

  implicit val hasId: HasId[Asset] = (target: Asset) => target.id
  implicit val hasModelName: NamedModel[Asset] = _ => modelName
}
