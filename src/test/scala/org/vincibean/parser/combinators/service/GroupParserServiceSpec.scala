package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.vincibean.parser.combinators.lexical.{Ast, Group}

class GroupParserServiceSpec
    extends Specification
    with ScalaCheck
    with GroupParserService {

  override def is: SpecStructure =
    s2"""
        GroupParserService can
          parse any header of any Group $p1
    """

  private val p1 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (s: String) =>
      val input = s"[$s]\nkey = 42\n"
      val res = groupParser.parse(input)
      (res must beAnInstanceOf[Parsed.Success[Group[Ast.Val[_]]]]) and (res.get.value.header.name must beEqualTo(
        s))
    }
  }

}
