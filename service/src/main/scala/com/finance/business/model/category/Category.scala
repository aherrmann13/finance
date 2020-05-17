package com.finance.business.model.category

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Category(
    id: Option[Id],
    parentId: Option[Id],
    name: Name,
    description: Description,
    effectiveTime: Seq[DateRange],
    budget: Seq[Budget]
) extends Model
