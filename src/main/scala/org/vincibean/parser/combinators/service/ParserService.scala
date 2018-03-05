package org.vincibean.parser.combinators.service

import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical._

import resource._

import scala.io.Source

trait ParserService extends ConfigParserService {

  def loadConfig[V <: Ast.Val[_]](
      filePath: String,
      overrides: List[(String, String)]): Config[Ast.Val[_]] = {
    managed(Source.fromFile(filePath))
      .map { input =>
        val fileContents = input.getLines.map(_ + "\n")
        configParser(overrides)
          .parseIterator(fileContents)
          .fold(
            (_, _, _) => None,
            (c, _) => Option(c)
          )
      }
      .opt
      .flatten
      .getOrElse(lexical.Config(Vector.empty, overrides))
  }

}
