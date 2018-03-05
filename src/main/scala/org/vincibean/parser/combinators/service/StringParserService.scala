package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait StringParserService {

  val stringValueParser: core.Parser[Ast.Str, Char, String] = {
    val quote = P("\"")
    val stringContent = P(CharsWhile(c => !"\"".contains(c)))
    P(quote ~ stringContent.rep.! ~ quote).map(s => Ast.Str(s))
  }

}
