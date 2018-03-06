package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import java.nio.file.{Paths, Path => JPath}

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class PathParserServiceSpec
    extends Specification
    with ScalaCheck
    with PathParserService {

  override def is =
    s2"""
        PathParserService can
          parse any given *nix path of alphanumeric folder names $p1
    """

  private val jPathGenerator: Gen[JPath] =
    Gen.listOf(Gen.alphaNumStr).map(ss => Paths.get("/", ss: _*))

  implicit val arbitraryJPath: Arbitrary[JPath] = Arbitrary(jPathGenerator)

  val p1 = prop { (a: JPath) =>
    val res = pathParser.parse(a.toString)
    (res must beAnInstanceOf[Parsed.Success[JPath]]) and (res.get.value.wrapped.toString must beEqualTo(
      a.toString))
  }

}
