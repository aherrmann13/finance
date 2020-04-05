package com.finance.business.model.category

import com.finance.business.model.types._

object implicits {
  private val categoryModelName = ModelName("Category")

  implicit val categoryHasId: HasId[Category] = (target: Category) => target.id
  implicit val categoryHasName: HasName[Category] = (target: Category) => target.name
  implicit val categoryHasDescription: HasDescription[Category] = (target: Category) => target.description
  implicit val categoryHasModelName: NamedModel[Category] = new NamedModel[Category] {
    override def modelName: ModelName = categoryModelName
  }
}
