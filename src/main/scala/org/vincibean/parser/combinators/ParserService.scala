package org.vincibean.parser.combinators

import java.nio.file.Path

import fastparse.all._
import fastparse.{all, core}
import org.vincibean.parser.combinators.lexical._

import scala.io.Source

trait ParserService {

  // Spaces
  private val newline = P(StringIn("\n", "\r\n", "\r", "\f"))
  private val space = P(" " | "\t" | newline)
  private val specialChar = " <>\";/[]=\r\n"
  private val strChars = P(CharsWhile(c => !specialChar.contains(c)))
  private val string = P(strChars.rep.!)

  private val stringValueParser: core.Parser[Ast.Str, Char, String] = {
    val quote = P("\"")
    val stringContent = P(CharsWhile(c => !"\"".contains(c)))
    P(quote ~ stringContent.! ~ quote).map(s => Ast.Str(s))
  }

  private val numberParser: core.Parser[Ast.Num, Char, String] = {
    val digits = P(CharsWhileIn('0' to '9'))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)
    P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
      x => Ast.Num(x.toDouble)
    )
  }

  // TODO 0 & 1 shouldn't be considered numbers!
  private val booleanParser: core.Parser[Ast.SingleVal[Boolean], Char, String] =
    P(StringIn("true", "false", "yes", "no", "0", "1").!).map {
      case "true" | "yes" | "1" => Ast.True
      case "false" | "no" | "0" => Ast.False
    }

  private val pathParser: core.Parser[Ast.Path[Path], Char, String] = {
    val slash = P("/")
    (slash ~/ string).rep(1).map { x =>
      val fs = x.filter(_.nonEmpty)
      val path =
        if (fs.isEmpty)
          java.nio.file.Paths.get("/")
        else
          java.nio.file.Paths.get("/", fs: _*)
      Ast.Path(path)
    }
  }

  private val arrayParser
    : core.Parser[Ast.Arr[Ast.SingleVal[_]], Char, String] = {
    val el = P(booleanParser | numberParser | pathParser | stringValueParser)
    P(el.rep(sep = ",")).map(els => Ast.Arr(els.toList))
  }

  private val commentParser: all.Parser[Unit] = {
    val semicolon = P(";")
    val commentString = P(CharsWhile(c => !"\r\n\f".contains(c)))
    P(semicolon ~ commentString)
  }

  private val lineParser: all.Parser[Option[(Key, Ast.Val[_])]] = {
    val keyValue = string.!
    val ovride = P("<" ~/ string.! ~/ ">")
    val key: core.Parser[Key, Char, String] = P(keyValue.! ~ ovride.?).map {
      case (k, ovr) => Key(k, ovr)
    }
    val eq = P(space.rep(1) ~/ "=" ~/ space.rep(1))
    val value: all.Parser[Ast.Val[_]] = P(
      booleanParser | numberParser | pathParser | arrayParser | stringValueParser)
    val keyValueLine: all.Parser[(Key, Ast.Val[_])] = P(
      (key ~ eq ~ value) ~/ space.rep(1) ~/ commentParser.?)
    P((keyValueLine | commentParser) ~ newline.rep).map {
      case x: (_, _) => Option(x.asInstanceOf[(Key, Ast.Val[_])])
      case _         => None
    }
  }

  private val groupParser: core.Parser[Group[Ast.Val[_]], Char, String] = {
    val multiline: core.Parser[Map[Key, Ast.Val[_]], Char, String] =
      lineParser.rep.map(_.flatten.toMap)
    val header: core.Parser[Header, Char, String] =
      P("[" ~/ string ~/ "]" ~/ newline.rep(1)).map(h => Header(h))
    P(header ~/ multiline ~/ newline.rep).map {
      case (h, kvs) =>
        lexical.Group(h, kvs)
    }
  }

  private val configParser: core.Parser[Config, Char, String] =
    groupParser.rep.map { x =>
      lexical.Config(x.toVector)
    }

  def loadConfig(filePath: String,
                 overrides: List[(String, String)]): Config = {
    val fileContent = Source.fromFile(filePath).getLines.map(_ + "\n")
    configParser
      .parseIterator(fileContent)
      .fold[Config](
        (_, _, _) => lexical.Config(Vector.empty[Group[_]]),
        (c: Config, _) => c
      )
  }

}
