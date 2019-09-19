package com.finance.business.model.source

import com.finance.business.common.{HasId, HasUserId}

object Source {
  def apply(userId: Int, name: String, description: String) =
    new Source(Option.empty, userId, name, description)
}

case class Source(id: Option[Int], userId: Int, name: String, description: String)
  extends HasId with HasUserId;
