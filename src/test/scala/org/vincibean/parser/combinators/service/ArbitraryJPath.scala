package org.vincibean.parser.combinators.service

import java.nio.file.{Paths, Path => JPath}

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryJPath {

  private val jPathGenerator: Gen[JPath] =
    Gen.listOf(Gen.alphaNumStr).map(ss => Paths.get("/", ss: _*))

  implicit val arbitraryJPath: Arbitrary[JPath] = Arbitrary(jPathGenerator)

}
