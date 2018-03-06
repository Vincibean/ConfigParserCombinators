package org.vincibean.parser.combinators.lexical

case class Group[V <: Ast.Val[_]](header: Header, settings: Seq[(Key, V)])
