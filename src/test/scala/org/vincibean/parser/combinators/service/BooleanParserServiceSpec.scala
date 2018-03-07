package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class BooleanParserServiceSpec
    extends Specification
    with ScalaCheck
    with BooleanParserService {

  override def is: SpecStructure =
    s2"""
        BooleanParserService can
          parse any of the following values: "true", "yes", "1" $p1
          parse any of the following values: "false", "no", "0" $p2
    """

  private val p1 = {
    implicit val a: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("true", "yes", "1")))
    prop { (a: String) =>
      val res = booleanParser.parse(a)
      (res must beAnInstanceOf[Parsed.Success[Boolean]]) and (res.get.value.wrapped must beTrue)
    }
  }

  private val p2 = {
    implicit val a: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("false", "no", "0")))
    prop { (a: String) =>
      val res = booleanParser.parse(a)
      (res must beAnInstanceOf[Parsed.Success[Boolean]]) and (res.get.value.wrapped must beFalse)
    }
  }

}
