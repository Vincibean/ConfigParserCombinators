package org.vincibean.parser.combinators.lexical

case class Group[V <: Ast.Val[_]](name: Header, settings: Seq[(Key, V)])
