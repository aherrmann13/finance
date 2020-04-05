package com.finance.business.model.payback

import com.finance.business.model.types._

object implicits {
  private val paybackModelName = ModelName("Payback")

  implicit val paybackHasId: HasId[Payback] = (target: Payback) => target.id
  implicit val paybackHasName: HasName[Payback] = (target: Payback) => target.name
  implicit val paybackHasDescription: HasDescription[Payback] = (target: Payback) => target.description
  implicit val paybackHasModelName: NamedModel[Payback] = new NamedModel[Payback] {
    override def modelName: ModelName = paybackModelName
  }
}
