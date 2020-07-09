package com.finance.service.converters

trait Mapping[A, B] {
  def mapTo(a: A): B
}

object Mapping {
  def apply[A, B](implicit tc: Mapping[A, B]): Mapping[A, B] = tc

  implicit class MappingOps[A](a: A) {
    def mapTo[B](implicit tc: Mapping[A, B]): B = tc.mapTo(a)
  }
}
