package com.finance.business.validation.errors

import com.finance.business.model.types.{Id, ModelName}

object DoesNotExist {
  def apply(modelName: ModelName): DoesNotExist = DoesNotExist(modelName, None)

  def apply(model: ModelName, id: Id): DoesNotExist = new DoesNotExist(model, Some(id))
}

case class DoesNotExist(model: ModelName, id: Option[Id]) extends ValidationError
