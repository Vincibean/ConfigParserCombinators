package org.vincibean.parser.combinators.service

import fastparse.all.Parsed.{Success => ParsedSuccess}
import java.nio.file.{Path => JPath}

import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import org.vincibean.parser.combinators.lexical.{Ast, Key}

class LineParserServiceSpec
    extends Specification
    with ScalaCheck
    with LineParserService
    with ArbitraryJPath {

  override def is =
    s2"""
        LineParserService can
          parse any line containing a String value $p1
          parse any line containing a Double value $p2
          parse any line containing a Boolean (true) value $p3
          parse any line containing a Boolean (true) value $p4
          parse any line containing a Path value $p5
          parse any line containing an override $p6
          parse any line containing a comment $p7
          parse any line containing only a comment $p8
    """

  val p1 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (k: String, v: String) =>
      (k.nonEmpty && v.nonEmpty) ==> {
        val quoted = s""""$v""""
        val input = toLine(k, quoted)
        checkParsed(lineParser.parse(input), k, v)
      }
    }
  }

  val p2 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (k: String, v: Double) =>
      k.nonEmpty ==> {
        val input = toLine(k, v)
        checkParsed(lineParser.parse(input), k, v)
      }
    }
  }

  val p3 = {
    implicit val bs: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("true", "yes", "1")))

    prop { (k: String, v: String) =>
      val input = toLine(k, v)
      val res = lineParser.parse(input)
      (res must beAnInstanceOf[ParsedSuccess[Option[(Key, Ast.Val[_])]]]) and
        (res.get.value must beSome.like {
          case (key: Key, value: Ast.Val[_]) =>
            (key.key must beEqualTo(k)) and (value.wrapped.toString must beEqualTo(
              true.toString))
        })
    }
  }

  val p4 = {
    implicit val bs: Arbitrary[String] = Arbitrary(
      Gen.oneOf(Seq("false", "no", "0")))

    prop { (k: String, v: String) =>
      val input = toLine(k, v)
      val res = lineParser.parse(input)
      (res must beAnInstanceOf[ParsedSuccess[Option[(Key, Ast.Val[_])]]]) and
        (res.get.value must beSome.like {
          case (key: Key, value: Ast.Val[_]) =>
            (key.key must beEqualTo(k)) and (value.wrapped.toString must beEqualTo(
              false.toString))
        })
    }
  }

  val p5 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (k: String, v: JPath) =>
      k.nonEmpty ==> {
        val input = toLine(k, v)
        checkParsed(lineParser.parse(input), k, v)
      }
    }
  }

  val p6 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (k: String, o: String, v: String) =>
      (k.nonEmpty && v.nonEmpty) ==> {
        val quoted = s""""$v""""
        val input = s"$k<$o> = $quoted \n"
        val res = lineParser.parse(input)
        (res must beAnInstanceOf[ParsedSuccess[Option[(Key, Ast.Val[_])]]]) and
          (res.get.value must beSome.like {
            case (key: Key, value: Ast.Val[_]) =>
              (key.key must beEqualTo(k)) and (key.ovride must beSome(o)) and (value.wrapped must beEqualTo(
                v))
          })
      }
    }
  }

  val p7 = {
    implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

    prop { (k: String, v: Double) =>
      k.nonEmpty ==> {
        val input = s"$k = $v ; some comment here \n"
        checkParsed(lineParser.parse(input), k, v)
      }
    }
  }

  val p8 = prop { (c: String) =>
    val input = s"; $c \n"
    val res = lineParser.parse(input)
    (res must beAnInstanceOf[ParsedSuccess[Option[(Key, Ast.Val[_])]]]) and
      (res.get.value must beNone)
  }

  private def toLine[V](k: String, v: V): String = s"$k = $v \n"

  private def checkParsed[V](
      res: Parsed[Option[(Key, Ast.Val[_])], Char, String],
      k: String,
      v: V) =
    (res must beAnInstanceOf[ParsedSuccess[Option[(Key, Ast.Val[_])]]]) and
      (res.get.value must beSome.like {
        case (key: Key, value: Ast.Val[_]) =>
          (key.key must beEqualTo(k)) and (value.wrapped must beEqualTo(v))
      })

}
