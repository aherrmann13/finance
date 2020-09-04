package com.finance.business.instances

import cats.Invariant

object NumericInstances {
  implicit val invariantForNumeric: Invariant[Numeric] =
    new Invariant[Numeric] {
      override def imap[A, B](fa: Numeric[A])(f: A => B)(g: B => A): Numeric[B] =
        new Numeric[B] {
          override def plus(x: B, y: B): B = f(fa.plus(g(x), g(y)))

          override def minus(x: B, y: B): B = f(fa.minus(g(x), g(y)))

          override def times(x: B, y: B): B = f(fa.times(g(x), g(y)))

          override def negate(x: B): B = f(fa.negate(g(x)))

          override def fromInt(x: Int): B = f(fa.fromInt(x))

          override def parseString(str: String): Option[B] = fa.parseString(str).map(f)

          override def toInt(x: B): Int = fa.toInt(g(x))

          override def toLong(x: B): Long = fa.toLong(g(x))

          override def toFloat(x: B): Float = fa.toFloat(g(x))

          override def toDouble(x: B): Double = fa.toDouble(g(x))

          override def compare(x: B, y: B): Int = fa.compare(g(x), g(y))
        }
    }
}
