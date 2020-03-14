package com.finance.business.model.payback

import com.finance.business.model.base.Model
import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Payback(id: Option[Id], name: Name, description: Description, date: DateTime) extends Model
