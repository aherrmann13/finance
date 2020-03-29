package com.finance.business.model.types

trait HasDescription[A] {
  def description(target: A): Description
}

object HasDescription {
  def apply[A](implicit tc: HasDescription[A]): HasDescription[A] = tc

  object ops {
    implicit class HasDescriptionOps[A: HasDescription](target: A) {
      def description: Description = HasDescription[A].description(target)
    }
  }
}
