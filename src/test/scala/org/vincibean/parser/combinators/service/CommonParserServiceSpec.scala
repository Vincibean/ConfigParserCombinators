package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class CommonParserServiceSpec
    extends Specification
    with ScalaCheck
    with CommonParserService {

  override def is: SpecStructure =
    s2"""
        CommonParserService can
          parse any newline character $p1
          parse any space character $p2
          parse any allowed string $p3
          parse any commented string $p4
    """

  private val p1 = {
    implicit val a: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("\n", "\r\n", "\r", "\f")))
    prop { (a: String) =>
      newline.parse(a) must beAnInstanceOf[Parsed.Success[Unit]]
    }
  }

  private val p2 = {
    implicit val a: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("\n", "\r\n", "\r", "\f", " ", "\t")))
    prop { (a: String) =>
      space.parse(a) must beAnInstanceOf[Parsed.Success[Unit]]
    }
  }

  private val p3 = prop { (a: String) =>
    (!specialChar.contains(a)) ==>
      (string.parse(a) must beAnInstanceOf[Parsed.Success[Unit]])
  }

  private val p4 = prop { (a: String) =>
    string.parse(";" + a) must beAnInstanceOf[Parsed.Success[Unit]]
  }

}
