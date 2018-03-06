package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait BooleanParserService {

  val booleanParser: core.Parser[Ast.SingleVal[Boolean], Char, String] =
    P(StringIn("true", "false", "yes", "no", "0", "1").!).map {
      case "true" | "yes" | "1" => Ast.True
      case "false" | "no" | "0" => Ast.False
    }

}
