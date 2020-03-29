package com.finance.business.model.types

trait HasName[A] {
  def name(target: A): Name
}

object HasName {
  def apply[A](implicit tc: HasName[A]): HasName[A] = tc

  object ops {
    implicit class HasNameOps[A: HasName](target: A) {
      def name: Name = HasName[A].name(target)
    }
  }
}
