package com.finance.business.model.category

import com.finance.business.model.types._

case class Category(
    id: Id,
    name: Name,
    description: Description,
    effectiveTime: EffectiveTime,
    budget: Seq[Budget]
)
