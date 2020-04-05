package com.finance.business.model.account

import com.finance.business.model.types._

object implicits {
  private val accountModelName = ModelName("Account")

  implicit val accountHasId: HasId[Account] = (target: Account) => target.id
  implicit val accountHasName: HasName[Account] = (target: Account) => target.name
  implicit val accountHasDescription: HasDescription[Account] = (target: Account) => target.description
  implicit val accountHasModelName: NamedModel[Account] = new NamedModel[Account] {
    override def modelName: ModelName = accountModelName
  }
}
