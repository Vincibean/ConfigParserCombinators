package org.vincibean.parser.combinators.service

import fastparse.all.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.vincibean.parser.combinators.lexical.{Ast, Config}

class ConfigParserServiceSpec
    extends Specification
    with ScalaCheck
    with ConfigParserService {

  override def is: SpecStructure =
    s2"""
        GroupParserService can (no overrides)
          successfully parse any Group $p1
          allow dynamic retrieval (only in group + key case) $p2
        GroupParserService can (with overrides)
          successfully parse any Group with the preferred override $p3
          successfully parse any Group with the fallback override $p4
          allow dynamic retrieval with the preferred override (only in group + key case) $p5
          allow dynamic retrieval with the fallback override (only in group + key case) $p6
          allow retrieval with the dot notation $p7
          allow retrieval with the square brackets notation $p8
          allow retrieval of a Map when given only the group name $p8
    """

  private implicit val s: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

  private val p1 =
    prop { (s: String, k: String, v: Double) =>
      (s.nonEmpty && k.nonEmpty) ==> {
        val input = s"[$s]\n$k = $v\n"
        val res = configParser(Nil).parse(input)
        res must beAnInstanceOf[Parsed.Success[Config[Ast.Val[_]]]]
      }
    }

  private val p2 = prop { (v: Double) =>
    val input = s"[mygroup]\nmykey = $v\n"
    val config = configParser(Nil).parse(input).get.value
    config.mygroup.mykey must beEqualTo(v)
  }

  private val p3 = prop { (s: String, k: String, o: String, v: Double) =>
    (s.nonEmpty && k.nonEmpty) ==> {
      val input = s"[$s]\n$k<$o> = $v\n$k = ${v + 1}\n"
      val res = configParser(List(o -> "i-should-not-exist")).parse(input)
      res must beAnInstanceOf[Parsed.Success[Config[Ast.Val[_]]]]
    }
  }

  private val p4 = prop { (s: String, k: String, o: String, v: Double) =>
    (s.nonEmpty && k.nonEmpty) ==> {
      val input = s"[$s]\n$k<$o> = $v\n$k = ${v + 1}\n"
      val res = configParser(List("i-should-not-exist" -> o)).parse(input)
      res must beAnInstanceOf[Parsed.Success[Config[Ast.Val[_]]]]
    }
  }

  private val p5 = prop { (o: String, v: Double) =>
    val input = s"[mygroup]\nmykey<$o> = $v\nmykey = ${v + 1}\n"
    val config =
      configParser(List(o -> "i-should-not-exist")).parse(input).get.value
    config.mygroup.mykey must beEqualTo(v)
  }

  private val p6 = prop { (o: String, v: Double) =>
    val input = s"[mygroup]\nmykey<$o> = $v\nmykey = ${v + 1}\n"
    val config =
      configParser(List("i-should-not-exist" -> o)).parse(input).get.value
    config.mygroup.mykey must beEqualTo(v)
  }

  private val p7 = prop { (s: String, k: String, o: String, v: Double) =>
    (s.nonEmpty && k.nonEmpty) ==> {
      val input = s"[$s]\n$k<$o> = $v\n$k = ${v + 1}\n"
      val config =
        configParser(List("i-should-not-exist" -> o)).parse(input).get.value
      config(s"$s.$k") must beEqualTo(v)
    }
  }

  private val p8 = prop { (s: String, k: String, o: String, v: Double) =>
    (s.nonEmpty && k.nonEmpty) ==> {
      val input = s"[$s]\n$k<$o> = $v\n$k = ${v + 1}\n"
      val config =
        configParser(List("i-should-not-exist" -> o)).parse(input).get.value
      config(s"$s[‘$k’]") must beEqualTo(v)
    }
  }

  private val p9 = prop { (s: String, k: String, o: String, v: Double) =>
    (s.nonEmpty && k.nonEmpty) ==> {
      val input = s"[$s]\n$k<$o> = $v\n$k = ${v + 1}\n"
      val config =
        configParser(List("i-should-not-exist" -> o)).parse(input).get.value
      config(s"$s") must beEqualTo(Map(s"$k" -> v))
    }
  }

}
