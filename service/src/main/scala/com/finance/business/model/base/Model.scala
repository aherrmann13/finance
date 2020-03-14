package com.finance.business.model.base

import com.finance.business.model.types.Id

trait Model {
  val id: Option[Id]
}
