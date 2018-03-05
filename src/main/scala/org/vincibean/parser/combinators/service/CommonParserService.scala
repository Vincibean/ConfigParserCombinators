package org.vincibean.parser.combinators.service

import fastparse.all
import fastparse.all._

trait CommonParserService {
  // Spaces
  val newline: all.Parser[Unit] = P(StringIn("\n", "\r\n", "\r", "\f"))
  val space: all.Parser[Unit] = P(" " | "\t" | newline)
  val specialChar = " <>\";/[]=\r\n"
  private val strChars = P(CharsWhile(c => !specialChar.contains(c)))
  val string: all.Parser[String] = P(strChars.rep.!)

  val commentParser: all.Parser[Unit] = {
    val semicolon = P(";")
    val commentString = P(CharsWhile(c => !"\r\n\f".contains(c)))
    P(semicolon ~ commentString)
  }

}
