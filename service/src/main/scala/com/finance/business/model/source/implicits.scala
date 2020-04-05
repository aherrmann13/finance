package com.finance.business.model.source

import com.finance.business.model.types._

object implicits {
  private val sourceModelName = ModelName("Source")

  implicit val sourceHasId: HasId[Source] = (target: Source) => target.id
  implicit val sourceHasName: HasName[Source] = (target: Source) => target.name
  implicit val sourceHasDescription: HasDescription[Source] = (target: Source) => target.description
  implicit val sourceHasModelName: NamedModel[Source] = new NamedModel[Source] {
    override def modelName: ModelName = sourceModelName
  }
}
