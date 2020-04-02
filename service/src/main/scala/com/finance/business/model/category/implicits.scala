package com.finance.business.model.category

import com.finance.business.model.types._

object implicits {
  private val modelName = ModelName("Category")

  implicit val hasId: HasId[Category] = (target: Category) => target.id
  implicit val hasName: HasName[Category] = (target: Category) => target.name
  implicit val hasDescription: HasDescription[Category] = (target: Category) => target.description
  implicit val hasModelName: NamedModel[Category] = _ => modelName
}
