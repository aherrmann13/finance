package com.finance.business.model.account

import com.finance.business.model.types._

object implicits {
  private val modelName = ModelName("Account")

  implicit val hasId: HasId[Account] = (target: Account) => target.id
  implicit val hasName: HasName[Account] = (target: Account) => target.name
  implicit val hasDescription: HasDescription[Account] = (target: Account) => target.description
  implicit val hasModelName: NamedModel[Account] = _ => modelName
}
