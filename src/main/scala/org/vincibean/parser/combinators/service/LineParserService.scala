package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.{all, core}
import org.vincibean.parser.combinators.lexical.{Ast, Key}

trait LineParserService
    extends StringParserService
    with NumberParserService
    with BooleanParserService
    with PathParserService
    with ArrayParserService {

  val lineParser: all.Parser[Option[(Key, Ast.Val[_])]] = {
    val keyValue = string.!
    val ovride = P("<" ~/ string.! ~/ ">")
    val key: core.Parser[Key, Char, String] = P(keyValue.! ~ ovride.?).map {
      case (k, ovr) => Key(k, ovr)
    }
    val eq = P(space.rep(1) ~/ "=" ~/ space.rep(1))
    val value: all.Parser[Ast.Val[_]] = P(
      booleanParser | numberParser | pathParser | arrayParser | stringValueParser)
    val keyValueLine: all.Parser[(Key, Ast.Val[_])] = P(
      (key ~ eq ~ value) ~/ space.rep(1) ~/ commentParser.?)
    P((keyValueLine | commentParser) ~ newline.rep).map {
      case x: (_, _) => Option(x.asInstanceOf[(Key, Ast.Val[_])])
      case _         => None
    }
  }

}
