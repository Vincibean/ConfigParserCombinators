package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait ArrayParserService
    extends BooleanParserService
    with NumberParserService
    with PathParserService
    with StringParserService {

  val arrayParser: core.Parser[Ast.Arr[Ast.SingleVal[_]], Char, String] = {
    val el = P(booleanParser | numberParser | pathParser | stringValueParser)
    P(el.rep(min = 2, sep = ",")).map(els => Ast.Arr(els.toList))
  }

}
