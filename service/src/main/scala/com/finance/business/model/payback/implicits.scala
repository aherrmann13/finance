package com.finance.business.model.payback

import com.finance.business.model.types._

object implicits {
  private val modelName = ModelName("Payback")

  implicit val hasId: HasId[Payback] = (target: Payback) => target.id
  implicit val hasName: HasName[Payback] = (target: Payback) => target.name
  implicit val hasDescription: HasDescription[Payback] = (target: Payback) => target.description
  implicit val hasModelName: NamedModel[Payback] = _ => modelName
}
