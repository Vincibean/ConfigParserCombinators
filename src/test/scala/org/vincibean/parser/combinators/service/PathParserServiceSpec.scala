package org.vincibean.parser.combinators.service

import java.nio.file.{Path => JPath}

import fastparse.all.Parsed
import org.specs2.{ScalaCheck, Specification}

class PathParserServiceSpec
    extends Specification
    with ScalaCheck
    with PathParserService
    with ArbitraryJPath {

  override def is =
    s2"""
        PathParserService can
          parse any given *nix path of alphanumeric folder names $p1
    """

  val p1 = prop { (a: JPath) =>
    val res = pathParser.parse(a.toString)
    (res must beAnInstanceOf[Parsed.Success[JPath]]) and (res.get.value.wrapped.toString must beEqualTo(
      a.toString))
  }

}
