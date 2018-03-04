package org.vincibean.parser.combinators

import java.nio.file.Path

import fastparse.all._
import fastparse.{all, core}

import scala.io.Source

object Main {

  def main(args: Array[String]) {
    // TODO libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"
    println(
      loadConfig(
        "/Users/andrea/IdeaProjects/mine/Parser Combinators/src/main/resources/settings.conf",
        Nil))
  }

  // Space
  val newline = P("\n" | "\r\n" | "\r" | "\f")
  val space = P(" " | "\t" | newline)

  // String
  val specialChar = " <>\";/[]=\r\n"
  val strChars = P(CharsWhile(c => !specialChar.contains(c)))
  val string = P(strChars.rep.!)
  val quote = P("\"")
  val stringContent = P(CharsWhile(c => !"\"".contains(c)))
  val stringValue: core.Parser[Ast.Str, Char, String] =
    P(quote ~ stringContent.! ~ quote).map(s => Ast.Str(s))

  // Number
  val digits = P(CharsWhileIn('0' to '9'))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val number: core.Parser[Ast.Num, Char, String] =
    P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => Ast.Num(x.toDouble)
    )

  // Boolean
  // TODO 0 & 1 shouldn't be considered numbers!
  val boolean: core.Parser[Ast.SingleVal[Boolean], Char, String] =
    P(StringIn("true", "false", "yes", "no", "0", "1").!).map {
      case "true" | "yes" | "1" => Ast.True
      case "false" | "no" | "0" => Ast.False
    }

  // Path
  val slash = P("/")
  val path: core.Parser[Ast.Path[Path], Char, String] =
    (slash ~ string).rep(1).map { x =>
      val fs = x.filter(_.nonEmpty)
      val path =
        if (fs.isEmpty)
          java.nio.file.Paths.get("/")
        else
          java.nio.file.Paths.get("/", fs: _*)
      Ast.Path(path)
    }

  // Array
  val el = P(boolean | number | path | stringValue)
  val array: core.Parser[Ast.Arr[Ast.SingleVal[_]], Char, String] =
    P(el.rep(sep = ",")).map(els => Ast.Arr(els.toList))

  // Comment
  val semicolon = P(";")
  val commentString = P(CharsWhile(c => !"\r\n\f".contains(c)))
  val comment = P(semicolon ~ commentString)

  // Summary
  val keyValue = string.!
  val ovride = P("<" ~ string.! ~ ">")
  val key: core.Parser[Key, Char, String] = P(keyValue.! ~ ovride.?).map {
    case (k, ovr) => Key(k, ovr)
  }
  val eq = P(space.rep(1) ~ "=" ~ space.rep(1))
  val value: all.Parser[Ast.Val[_]] = P(
    boolean | number | path | array | stringValue)

  val keyValueLine: all.Parser[(Key, Ast.Val[_])] = P(
    key ~ eq ~ value ~ space.rep(1) ~ comment.?)
  val line: all.Parser[Option[(Key, Ast.Val[_])]] =
    P((keyValueLine | comment) ~ newline.rep).map {
      case x: (Key, Ast.Val[_]) => Some(x)
      case _                    => None
    }

  val multiline: core.Parser[Map[Key, Ast.Val[_]], Char, String] =
    line.rep.map(_.flatten.toMap)

  val header: core.Parser[Header, Char, String] =
    P("[" ~ string ~ "]" ~ newline.rep(1)).map(h => Header(h))

  val total: core.Parser[Group[Ast.Val[_]], Char, String] =
    P(header ~ multiline ~ newline.rep).map {
      case (h, kvs) =>
        Group(h, kvs)
    }

  val multitotal: core.Parser[Config, Char, String] = total.rep.map { x =>
    Config(x: _*)
  }

  def loadConfig(
      filePath: String,
      overrides: List[(String, String)]): core.Parsed[Config, Char, String] = {
    multitotal
      .parseIterator(
        Source
          .fromFile(
            "/Users/andrea/IdeaProjects/mine/Parser Combinators/src/main/resources/settings.conf")
          .getLines
          .map(_ + "\n"))
  }

}
