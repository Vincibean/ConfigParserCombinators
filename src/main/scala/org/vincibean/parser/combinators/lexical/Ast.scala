package org.vincibean.parser.combinators.lexical

import java.nio.file.{Path => JPath}
import scala.collection.immutable.Seq

object Ast {

  sealed trait Val[A] {
    def wrapped: A
  }

  sealed trait SingleVal[A] extends Val[A]

  final case class Str(wrapped: String) extends SingleVal[String]

  final case class Num(wrapped: Double) extends SingleVal[Double]

  final case class Path[T <: JPath](wrapped: T) extends SingleVal[T]

  final case object False extends SingleVal[Boolean] {
    def wrapped: Boolean = false
  }

  final case object True extends SingleVal[Boolean] {
    def wrapped: Boolean = true
  }

  final case class Arr[V <: SingleVal[_]](wrapped: Seq[V]) extends Val[Seq[V]]

}
