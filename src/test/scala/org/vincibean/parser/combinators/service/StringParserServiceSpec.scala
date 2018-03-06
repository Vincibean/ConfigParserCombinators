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
    val res = stringValueParser.parse(s""""$a"""")
    (res must beAnInstanceOf[Parsed.Success[String]]) and (res.get.value.wrapped must beEqualTo(
      a))
  }

}
