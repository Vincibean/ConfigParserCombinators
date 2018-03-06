package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait BooleanParserService {

  val booleanParser: core.Parser[Ast.SingleVal[Boolean], Char, String] = {
    val number = P(CharsWhileIn(('0' to '9') :+ '.'))
    val as0 = P("0" ~ !number)
    val as1 = P("1" ~ !number)
    val others = P(StringIn("true", "false", "yes", "no"))
    P(others | as0 | as1).!.map {
      case "true" | "yes" | "1" => Ast.True
      case "false" | "no" | "0" => Ast.False
    }
  }

}
