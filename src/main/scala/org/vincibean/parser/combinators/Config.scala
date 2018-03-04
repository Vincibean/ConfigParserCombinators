package org.vincibean.parser.combinators

import scala.collection.immutable.Seq

case class Config(settings: Seq[Group[_]]) extends AnyVal
