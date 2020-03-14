package com.finance.business.model.category

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Category(
    id: Option[Id],
    name: Name,
    description: Description,
    effectiveTime: EffectiveTime,
    budget: Seq[Budget]
) extends Model
