package com.finance.business.model.payback

import java.time.OffsetDateTime

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Payback(id: Option[Id], name: Name, description: Description, date: OffsetDateTime) extends Model
