package com.finance.business.model.types

trait NamedModel[A] {
  def modelName(target: A): ModelName
}

object NamedModel {
  def apply[A](implicit tc: NamedModel[A]): NamedModel[A] = tc

  object ops {
    implicit class NamedModelOps[A: NamedModel](target: A) {
      def modelName: ModelName = NamedModel[A].modelName(target)
    }
  }
}
