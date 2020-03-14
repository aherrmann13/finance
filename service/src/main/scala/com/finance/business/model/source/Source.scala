package com.finance.business.model.source

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Source(id: Option[Id], name: Name, description: Description) extends Model
