package org.vincibean.parser.combinators.service

import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical._

import scala.io.Source

trait ParserService extends ConfigParserService {

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
