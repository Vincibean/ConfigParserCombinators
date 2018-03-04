package org.vincibean.parser.combinators

import scala.collection.immutable.Seq

object Ast {

  sealed trait Val[A] {
    def value: A
  }

  sealed trait SingleVal[A] extends Val[A]

  final case class Str(value: String) extends SingleVal[String]

  final case class Num(value: Double) extends SingleVal[Double]

  final case class Path[T <: java.nio.file.Path](value: T) extends SingleVal[T]

  final case object False extends SingleVal[Boolean] {
    def value: Boolean = false
  }

  final case object True extends SingleVal[Boolean] {
    def value: Boolean = true
  }

  final case class Arr[V <: SingleVal[_]](value: Seq[V]) extends Val[Seq[V]]

}
