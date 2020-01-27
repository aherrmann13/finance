package com.finance.business.model.category

sealed trait EffectiveTime;


case object Always extends EffectiveTime
case class Single(period: Int) extends EffectiveTime
case class Collection(periods: Seq[Int]) extends EffectiveTime
case class Range(from: Int, to: Int) extends EffectiveTime