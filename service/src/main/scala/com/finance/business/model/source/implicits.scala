package com.finance.business.model.source

import com.finance.business.model.types._

object implicits {
  private val modelName = ModelName("Source")

  implicit val hasId: HasId[Source] = (target: Source) => target.id
  implicit val hasName: HasName[Source] = (target: Source) => target.name
  implicit val hasDescription: HasDescription[Source] = (target: Source) => target.description
  implicit val hasModelName: NamedModel[Source] = _ => modelName
}
