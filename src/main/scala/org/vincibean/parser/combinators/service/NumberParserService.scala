package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait NumberParserService {

  val numberParser: core.Parser[Ast.Num, Char, String] = {
    val digits = P(CharsWhileIn('0' to '9'))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)
    P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => Ast.Num(x.toDouble)
    )
  }

}
