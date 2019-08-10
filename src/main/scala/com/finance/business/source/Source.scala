package com.finance.business.source

import com.finance.business.common.HasId

object Source {
  def maxNameLength = 128
  def maxDescriptionLength = 512

  def apply(userId: Int, name: String, description: String) =
    new Source(Option.empty, userId, name, description)
}

case class Source(id: Option[Int], userId: Int, name: String, description: String) extends HasId
