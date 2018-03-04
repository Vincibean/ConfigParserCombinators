package org.vincibean.parser.combinators

case class Group[V <: Ast.Val[_]](name: Header,
                                  private val settings: Map[Key, V])