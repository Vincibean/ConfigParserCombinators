package org.vincibean.parser.combinators

import fastparse.all._

import scala.io.Source

object Main {

  def main(args: Array[String]) {

    // Space
    val newline = P("\n" | "\r\n" | "\r" | "\f")
    val space = P(" " | "\t" | newline)

    // String
    val specialChar = " /[]=\r\n"
    val strChars = P(CharsWhile(c => !specialChar.contains(c)))
    val string = P(strChars.rep.!)

    // Number
    val digits = P(CharsWhileIn('0' to '9'))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)

    val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => x.toDouble
    )

    // Boolean
    val `true` = P("true")
    val `false` = P("false")
    val yes = P("yes")
    val no = P("no")
    // TODO 0 & 1 shouldn't be considered numbers!
    val as0 = P("0")
    val as1 = P("1")

    val boolean = P((`true` | `false` | yes | no | as0 | as1).!).map {
      case "true" | "yes" | "1" => true
      case "false" | "no" | "0" => false
    }

    // Path
    val slash = P("/")
    val path =
      (slash ~ string).rep.map(x => x.filter(_.nonEmpty))

    // Summary
    val keyValue = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_')).rep.!
    val key = P(keyValue.!)
    val eq = space ~ "=" ~ space
    val value = boolean | number | path | string

    val line = P(key ~ eq ~ value ~ newline.rep(1))

    val multiline = line.rep

    val header = P("[" ~ string ~ "]" ~ newline.rep(1))

    val total = P(header ~ multiline ~ newline.rep)

    val multitotal = total.rep

    val mys = P(AnyChar.rep.!)

    // TODO libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"
    println(
      multitotal.parseIterator(
        Source
          .fromFile(
            "/Users/andrea/IdeaProjects/mine/Parser Combinators/src/main/resources/test_settings.conf")
          .getLines
          .map(_ + "\n")))

  }

}

case class Key(ovride: String) extends AnyVal
case class Value[A](value: A) extends AnyVal
