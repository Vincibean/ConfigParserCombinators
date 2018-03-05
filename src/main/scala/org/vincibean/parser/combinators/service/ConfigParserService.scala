package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical.{Ast, Config}

trait ConfigParserService extends GroupParserService {

  def configParser[V <: Ast.Val[_]](overrides: List[(String, String)])
    : core.Parser[Config[Ast.Val[_]], Char, String] =
    groupParser.rep.map { x =>
      new lexical.Config(x.toVector, overrides)
    }

}
