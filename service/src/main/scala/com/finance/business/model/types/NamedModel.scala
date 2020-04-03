package com.finance.business.model.types

trait NamedModel[A] {
  def modelName: ModelName
}

object NamedModel {
  def apply[A](implicit tc: NamedModel[A]): NamedModel[A] = tc
}
