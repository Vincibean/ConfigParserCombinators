package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical.Config

trait ConfigParserService extends GroupParserService {

  val configParser: core.Parser[Config, Char, String] =
    groupParser.rep.map { x =>
      lexical.Config(x.toVector)
    }

}
