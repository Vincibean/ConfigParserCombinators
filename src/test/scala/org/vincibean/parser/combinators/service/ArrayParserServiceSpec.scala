package org.vincibean.parser.combinators.service

import java.nio.file.{Path => JPath}

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class ArrayParserServiceSpec
    extends Specification
    with ScalaCheck
    with ArrayParserService
    with ArbitraryJPath {

  override def is =
    s2"""
        ArrayParserService can
          parse any String array $p1
          parse any Double array $p2
          parse any Boolean array $p2
          parse any Path array $p2
    """

  val p1 = {
    implicit val ass: Arbitrary[Seq[String]] = Arbitrary(
      Gen.listOf(Gen.alphaNumStr))
    prop { (a: Seq[String]) =>
      val input = a.map(s => s""""$s"""").mkString(",")
      val res = arrayParser.parse(input)
      (res must beAnInstanceOf[Parsed.Success[Seq[String]]]) and (res.get.value.wrapped
        .map(_.wrapped) must beEqualTo(a))
    }
  }

  val p2 = {
    prop { (a: Seq[Double]) =>
      val res = arrayParser.parse(a.mkString(","))
      (res must beAnInstanceOf[Parsed.Success[Seq[Double]]]) and (res.get.value.wrapped
        .map(_.wrapped) must beEqualTo(a))
    }
  }

  val p3 = {
    implicit val ass: Arbitrary[Seq[String]] = Arbitrary(
      Gen.listOf(Gen.oneOf(Seq("true", "yes", "1", "false", "no", "0"))))
    prop { (a: Seq[String]) =>
      val res = arrayParser.parse(a.mkString(","))
      (res must beAnInstanceOf[Parsed.Success[Seq[Boolean]]]) and (res.get.value.wrapped
        .map(_.wrapped) must beEqualTo(a))
    }
  }

  val p4 = {
    prop { (a: Seq[JPath]) =>
      val res = arrayParser.parse(a.mkString(","))
      (res must beAnInstanceOf[Parsed.Success[Seq[JPath]]]) and (res.get.value.wrapped
        .map(_.wrapped) must beEqualTo(a))
    }
  }

}
