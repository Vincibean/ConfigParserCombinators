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
          parse any Boolean array (case of true) $p3
          parse any Boolean array (case of false) $p4
          parse any Path array $p5
    """

  val p1 = {
    implicit val strings: Arbitrary[Seq[String]] = Arbitrary(
      Gen.listOf(Gen.alphaNumStr))
    prop { (a: Seq[String]) =>
      (a.length > 1) ==> {
        val input = a.map(s => s""""$s"""").mkString(",")
        val res = arrayParser.parse(input)
        (res must beAnInstanceOf[Parsed.Success[Seq[String]]]) and (res.get.value.wrapped
          .map(_.wrapped) must beEqualTo(a))
      }
    }
  }

  val p2 = {
    prop { (a: Seq[Double]) =>
      (a.length > 1) ==> {
        val res = arrayParser.parse(a.mkString(","))
        (res must beAnInstanceOf[Parsed.Success[Seq[Double]]]) and (res.get.value.wrapped
          .map(_.wrapped) must beEqualTo(a))
      }
    }
  }

  val p3 = {
    implicit val bs: Arbitrary[Seq[String]] = Arbitrary(
      Gen.listOf(Gen.oneOf(Seq("true", "yes", "1"))))
    prop { (a: Seq[String]) =>
      (a.length > 1) ==> {
        val res = arrayParser.parse(a.mkString(","))
        (res must beAnInstanceOf[Parsed.Success[Seq[Boolean]]]) and (res.get.value.wrapped
          .map(_.wrapped)
          .map(_.asInstanceOf[Boolean])
          .reduce(_ && _) must beEqualTo(true))
      }
    }
  }

  val p4 = {
    implicit val bs: Arbitrary[Seq[String]] = Arbitrary(
      Gen.listOf(Gen.oneOf(Seq("false", "no", "0"))))
    prop { (a: Seq[String]) =>
      (a.length > 1) ==> {
        val res = arrayParser.parse(a.mkString(","))
        (res must beAnInstanceOf[Parsed.Success[Seq[Boolean]]]) and (res.get.value.wrapped
          .map(_.wrapped)
          .map(_.asInstanceOf[Boolean])
          .reduce(_ && _) must beEqualTo(false))
      }
    }
  }

  val p5 = {
    prop { (a: Seq[JPath]) =>
      (a.length > 1) ==> {
        val res = arrayParser.parse(a.mkString(","))
        (res must beAnInstanceOf[Parsed.Success[Seq[JPath]]]) and (res.get.value.wrapped
          .map(_.wrapped) must beEqualTo(a))
      }
    }
  }

}
