package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class BooleanParserServiceSpec
    extends Specification
    with ScalaCheck
    with BooleanParserService {

  override def is =
    s2"""
        BooleanParserService can
          parse any of the following values: "true", "false", "yes", "no", "0", "1" $p1
    """

  val p1 = {
    implicit val a: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("true", "false", "yes", "no", "0", "1")))
    prop { (a: String) =>
      booleanParser.parse(a) must beAnInstanceOf[Parsed.Success[Boolean]]
    }
  }

}
