package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.specs2.{ScalaCheck, Specification}

class StringParserServiceSpec
    extends Specification
    with ScalaCheck
    with StringParserService {

  override def is =
    s2"""
        StringParserService can
          parse any quoted string $p1
    """

  val p1 = prop { (a: String) =>
    stringValueParser.parse(s""""$a"""") must beAnInstanceOf[
      Parsed.Success[String]]
  }

}
