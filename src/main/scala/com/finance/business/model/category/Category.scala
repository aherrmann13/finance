package com.finance.business.model.category

import com.finance.business.common.{HasId, HasUserId}

object Category {
  def apply(
      userId: Int,
      parentId: Option[Int],
      name: String,
      description: String,
      effective: EffectiveTime,
      isLeaf: Boolean,
      budget: Seq[Budget]
  ): Category = new Category(None, userId, parentId, name, description, effective, isLeaf, budget)
}

case class Category(
    id: Option[Int],
    userId: Int,
    parentId: Option[Int],
    name: String,
    description: String,
    effective: EffectiveTime,
    isLeaf: Boolean,
    budget: Seq[Budget]
) extends HasId
    with HasUserId
