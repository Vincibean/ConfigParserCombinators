package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class NumberParserServiceSpec
    extends Specification
    with ScalaCheck
    with NumberParserService {

  override def is: SpecStructure =
    s2"""
        NumberParserService can
          parse any Double $p1
    """

  private val p1 = prop { (a: Double) =>
    (a != 0 && a != 1) ==> {
      val res = numberParser.parse(a.toString)
      (res must beAnInstanceOf[Parsed.Success[Double]]) and (res.get.value.wrapped must beEqualTo(
        a))
    }
  }

}
