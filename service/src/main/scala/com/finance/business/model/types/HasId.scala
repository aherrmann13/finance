package com.finance.business.model.types

trait HasId[A] {
  def id(target: A): Option[Id]
}

object HasId {
  def apply[A](implicit tc: HasId[A]): HasId[A] = tc

  object ops {
    implicit class HasIdOps[A: HasId](target: A) {
      def id: Option[Id] = HasId[A].id(target)
    }
  }
}
