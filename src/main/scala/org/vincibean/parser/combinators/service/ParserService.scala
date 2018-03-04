package org.vincibean.parser.combinators.service

import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical._

import scala.io.Source

trait ParserService extends ConfigParserService {

  def loadConfig[V <: Ast.Val[_]](
      filePath: String,
      overrides: List[(String, String)]): Config[Ast.Val[_]] = {
    val fileContent = Source.fromFile(filePath).getLines.map(_ + "\n")
    configParser(overrides)
      .parseIterator(fileContent)
      .fold(
        (_, _, _) => lexical.Config(Vector.empty, overrides),
        (c, _) => c
      )
  }

}
