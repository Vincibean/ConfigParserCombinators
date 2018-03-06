package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.specs2.{ScalaCheck, Specification}

class NumberParserServiceSpec
    extends Specification
    with ScalaCheck
    with NumberParserService {

  override def is =
    s2"""
        NumberParserService can
          parse any Double $p1
    """

  val p1 = prop { (a: Double) =>
    a != 0 && a != 1 ==>
      (numberParser.parse(a.toString) must beAnInstanceOf[
        Parsed.Success[String]])
  }

}
